// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate byteorder;

use byteorder::{BigEndian, ReadBytesExt, ByteOrder};

use std::fmt;
use std::io::{self, Read, Seek, SeekFrom, ErrorKind, Error};
use std::str::from_utf8;

/// The list of known atoms which can contain others.
const CONTAINER_ATOMS: [&'static [u8; 4]; 11] = [
    b"moov", b"udta", b"mdia", b"meta", b"ilst",
    b"stbl", b"minf", b"moof", b"traf", b"trak",
    b"stsd"
];

/// The basic unit of MP4 data. This is a lightweight handle that only contains seek positions of
/// data within a file.
#[derive(Default)]
pub struct Atom {
    /// The byte position in the parent file where this atom begins.
    pub start: u64,
    /// The number of bytes used by this atom. By seeking to `atom.start + atom.len`, you will
    /// reach the start of the next atom (or the end of the file).
    pub len: u64,
    /// The name of the atom. Usually this is four characters of ASCII text, but sometimes the byte
    /// `0xA9` is used for the first character, often rendered as the copyright symbol `©`.
    pub name: [u8; 4],
    /// The children of this atom, if any.
    pub children: Vec<Atom>,
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        struct Name([u8; 4]);

        impl fmt::Debug for Name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                if let Ok(name) = from_utf8(&self.0[..]) {
                    f.write_str(name)?;
                } else {
                    let slice = if self.0[0] == 0o251 {
                        f.write_str("©")?;
                        &self.0[1..]
                    } else {
                        &self.0[..]
                    };

                    if let Ok(suffix) = from_utf8(slice) {
                        f.write_str(suffix)?;
                    } else {
                        for &byte in slice {
                            f.write_str(&format!("\\x{:x}", byte))?;
                        }
                    }
                }

                Ok(())
            }
        }

        f.debug_struct("Atom")
            .field("start", &self.start)
            .field("len", &self.len)
            .field("name", &Name(self.name))
            .field("children", &self.children)
            .finish()
    }
}

/// Attempt to read an atom from the current position in the given buffer. If the end of the file
/// is reached, `Ok(None)` is returned.
fn read_atom<F: Read + Seek>(file: &mut F) -> io::Result<Option<Atom>> {
    let mut ret = Atom::default();
    ret.start = file.seek(SeekFrom::Current(0))?;

    let mut header = [0u8; 8];
    match file.read_exact(&mut header[..]) {
        Ok(()) => (),
        Err(ref e) if e.kind() == ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }

    ret.len = u64::from(BigEndian::read_u32(&header[..4]));

    if ret.len == 0 {
        // The last atom which extends to the end of the file
        ret.len = file.seek(SeekFrom::End(0))? - ret.start;
    } else if ret.len == 1 {
        // 64-bit atom length
        ret.len = file.read_u64::<BigEndian>()?;
    }

    if ret.len < 8 {
        //TODO: custom error enum
        return Err(Error::new(ErrorKind::InvalidData, "invalid atom size"));
    }

    ret.name = [header[4], header[5], header[6], header[7]];

    if CONTAINER_ATOMS.iter().any(|&cont| cont == &ret.name) {
        if &ret.name == b"meta" {
            file.seek(SeekFrom::Current(4))?;
        } else if &ret.name == b"stsd" {
            file.seek(SeekFrom::Current(8))?;
        }

        while file.seek(SeekFrom::Current(0))? < ret.start + ret.len {
            if let Some(child) = read_atom(file)? {
                ret.children.push(child);
            } else {
                return Ok(Some(ret));
            }
        }
    }

    file.seek(SeekFrom::Start(ret.start + ret.len))?;
    Ok(Some(ret))
}

/// Attempt to read all the atoms from the given buffer.
pub fn read_atoms<F: Read + Seek>(file: &mut F) -> io::Result<Vec<Atom>> {
    let end = file.seek(SeekFrom::End(0))?;
    file.seek(SeekFrom::Start(0))?;
    let mut ret = vec![];

    while let Some(atom) = read_atom(file)? {
        let done = atom.start + atom.len + 8 > end;
        ret.push(atom);

        if done { break; }
    }

    Ok(ret)
}

/// Given a list of previously-loaded `Atom`s, look up the atom at the given path.
pub fn find_atom<I: IntoIterator<Item=S>, S: AsRef<[u8]>>(atoms: &[Atom], path: I)
    -> Option<&Atom>
{
    fn inner<I: IntoIterator<Item=S>, S: AsRef<[u8]>>(atoms: &[Atom],
                                                      needle: Option<S>,
                                                      path_suffix: I)
        -> Option<&Atom>
    {
        if let Some(name) = needle {
            for a in atoms {
                if a.name == *name.as_ref() {
                    let mut iter = path_suffix.into_iter();
                    let next = iter.next();
                    if next.is_none() {
                        return Some(a);
                    } else {
                        return inner(&a.children, next, iter);
                    }
                }
            }
        }

        None
    }
    let mut iter = path.into_iter();
    let first = iter.next();

    inner(atoms, first, iter)
}

/// The loaded data corresponding to an `Atom`.
#[derive(Debug)]
pub struct AtomData {
    pub flags: i32,
    pub data: Vec<u8>,
}

/// Loads the given `Atom`'s data from the given buffer.
///
/// An atom that contains data will have a child `data` atom inside it. Fields that can contain
/// multiple values instead contain a sequence of these `data` atoms, represented here as the
/// `Vec<AtomData>` being returned.
fn load_atom_data<F: Read + Seek>(file: &mut F, atom: &Atom) -> io::Result<Vec<AtomData>> {
    file.seek(SeekFrom::Start(atom.start + 8))?;

    let mut ret = vec![];

    let mut pos = 0;
    let mut buf = vec![0u8; atom.len as usize];
    file.read_exact(&mut buf)?;

    while pos + 16 <= buf.len() {
        let len = BigEndian::read_u32(&buf[pos..]) as usize;
        if len < 12 {
            //atom too short
            //TODO: logging
            break;
        }

        let name = &buf[pos+4..pos+8];
        let flags = BigEndian::read_i32(&buf[pos+8..]);

        //TODO: freeform atoms

        if name != b"data" {
            //unexpected atom
            //TODO: logging
            break;
        }
        ret.push(AtomData { flags, data: buf[pos+16..pos+len].to_vec() });

        pos += len;
    }

    Ok(ret)
}

/// Loads the given `Atom` from the given buffer as a list of strings.
fn load_atom_string_list<F: Read + Seek>(file: &mut F, atom: &Atom)
    -> io::Result<Option<Vec<String>>>
{
    let mut ret = vec![];

    for data in load_atom_data(file, atom)? {
        if let Ok(text) = String::from_utf8(data.data) {
            ret.push(text);
        }
        //TODO: ...and what if it's not utf8?
    }

    if !ret.is_empty() {
        Ok(Some(ret))
    } else {
        Ok(None)
    }
}

/// Loads the given `Atom` from the given buffer as a string by first calling
/// `load_atom_string_list` and joining the strings together with the given separator.
fn load_atom_string<F: Read + Seek>(file: &mut F, atom: &Atom, sep: &str)
    -> io::Result<Option<String>>
{
    Ok(load_atom_string_list(file, atom)?.map(|l| l.join(sep)))
}

/// Loads the given `Atom` from the given buffer as two `i16`s.
fn load_atom_int_pair<F: Read + Seek>(file: &mut F, atom: &Atom)
    -> io::Result<Option<(i16, i16)>>
{
    Ok(load_atom_data(file, atom)?.first().map(|data| {
        (BigEndian::read_i16(&data.data[2..]), BigEndian::read_i16(&data.data[4..]))
    }))
}

#[derive(Debug, Default)]
pub struct Tags {
    pub artist: Option<String>,
    pub album: Option<String>,
    pub title: Option<String>,
    pub date: Option<String>,
    /// `(current_track, total_track_count)`
    pub track: Option<(i16, i16)>,
    /// `(current_disc, total_disc_count)`
    pub disc: Option<(i16, i16)>,
}

impl Tags {
    /// Loads the `Tags` from the given buffer.
    pub fn load<F: Read + Seek>(src: &mut F) -> io::Result<Tags> {
        let atoms = read_atoms(src)?;

        let mut ret = Tags::default();

        if let Some(ilst) = find_atom(&atoms, &["moov", "udta", "meta", "ilst"]) {
            for atom in &ilst.children {
                match &atom.name {
                    b"\xA9ART" => ret.artist = load_atom_string(src, atom, ", ")?,
                    b"\xA9alb" => ret.album = load_atom_string(src, atom, ", ")?,
                    b"\xA9nam" => ret.title = load_atom_string(src, atom, ", ")?,
                    b"\xA9day" => ret.date = load_atom_string(src, atom, " ")?,
                    b"trkn" => ret.track = load_atom_int_pair(src, atom)?,
                    b"disk" => ret.disc = load_atom_int_pair(src, atom)?,
                    _ => {} //TODO: unused atoms?
                }
            }
        }

        Ok(ret)
    }
}
