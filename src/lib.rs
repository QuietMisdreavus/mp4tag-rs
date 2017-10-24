// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern crate byteorder;

use byteorder::{BigEndian, ReadBytesExt, ByteOrder};

use std::fmt;
use std::io::{self, Read, Seek, SeekFrom, ErrorKind, Error};
use std::str::from_utf8;

const CONTAINER_ATOMS: [&'static [u8; 4]; 11] = [
    b"moov", b"udta", b"mdia", b"meta", b"ilst",
    b"stbl", b"minf", b"moof", b"traf", b"trak",
    b"stsd"
];

#[derive(Default)]
pub struct Atom {
    pub start: u64,
    pub len: u64,
    pub name: [u8; 4],
    pub children: Vec<Atom>,
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let name: String = if let Ok(name) = from_utf8(&self.name[..]) {
            name.to_string()
        } else {
            let mut name = String::new();

            let slice = if self.name[0] == 0o251 {
                name.push('©');
                &self.name[1..]
            } else {
                &self.name[..]
            };

            if let Ok(suffix) = from_utf8(slice) {
                name.push_str(suffix);
            } else {
                for &byte in slice {
                    name.push_str(&format!("\\x{:x}", byte));
                }
            }

            name
        };

        f.debug_struct("Atom")
            .field("start", &self.start)
            .field("len", &self.len)
            .field("name", &name)
            .field("children", &self.children)
            .finish()
    }
}

fn read_atom<F: Read + Seek>(file: &mut F) -> io::Result<Option<Atom>> {
    let mut ret = Atom::default();
    ret.start = file.seek(SeekFrom::Current(0))?;

    let mut header = [0u8; 8];
    match file.read_exact(&mut header[..]) {
        Ok(()) => (),
        Err(ref e) if e.kind() == ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    }

    ret.len = BigEndian::read_u32(&header[..4]) as u64;

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

pub fn find_atom<'a, I: IntoIterator<Item=S>, S: AsRef<[u8]>>(atoms: &'a [Atom], path: I)
    -> Option<&'a Atom>
{
    fn inner<'a, I: IntoIterator<Item=S>, S: AsRef<[u8]>>(atoms: &'a [Atom],
                                                          needle: Option<S>,
                                                          path_suffix: I)
        -> Option<&'a Atom>
    {
        if let Some(name) = needle {
            for a in atoms {
                if &a.name == name.as_ref() {
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

#[derive(Debug)]
pub struct AtomData {
    pub flags: i32,
    pub data: Vec<u8>,
}

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

fn load_atom_string<F: Read + Seek>(file: &mut F, atom: &Atom) -> io::Result<Option<String>> {
    let mut ret = String::new();

    for data in load_atom_data(file, atom)? {
        if let Ok(text) = String::from_utf8(data.data) {
            if !ret.is_empty() {
                ret.push(' ');
            }
            ret.push_str(&text);
        }
        //TODO: ...and what if it's not utf8?
    }

    if !ret.is_empty() {
        Ok(Some(ret))
    } else {
        Ok(None)
    }
}

pub fn date<F: Read + Seek>(file: &mut F, atoms: &[Atom]) -> io::Result<Option<String>> {
    if let Some(atom) = find_atom(atoms, &[b"moov", b"udta", b"meta", b"ilst", b"\xA9day"]) {
        load_atom_string(file, atom)
    } else {
        Ok(None)
    }
}

pub fn title<F: Read + Seek>(file: &mut F, atoms: &[Atom]) -> io::Result<Option<String>> {
    if let Some(atom) = find_atom(atoms, &[b"moov", b"udta", b"meta", b"ilst", b"\xA9nam"]) {
        load_atom_string(file, atom)
    } else {
        Ok(None)
    }
}

pub fn artist<F: Read + Seek>(file: &mut F, atoms: &[Atom]) -> io::Result<Option<String>> {
    if let Some(atom) = find_atom(atoms, &[b"moov", b"udta", b"meta", b"ilst", b"\xA9ART"]) {
        load_atom_string(file, atom)
    } else {
        Ok(None)
    }
}

pub fn album<F: Read + Seek>(file: &mut F, atoms: &[Atom]) -> io::Result<Option<String>> {
    if let Some(atom) = find_atom(atoms, &[b"moov", b"udta", b"meta", b"ilst", b"\xA9alb"]) {
        load_atom_string(file, atom)
    } else {
        Ok(None)
    }
}
