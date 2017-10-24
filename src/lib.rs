extern crate byteorder;
extern crate smallstring;

use byteorder::{BigEndian, ReadBytesExt, ByteOrder};
use smallstring::SmallString;

use std::io::{self, Read, Seek, SeekFrom, ErrorKind, Error};
use std::str::from_utf8;

const CONTAINER_ATOMS: [&'static str; 11] = [
    "moov", "udta", "mdia", "meta", "ilst",
    "stbl", "minf", "moof", "traf", "trak",
    "stsd"
];

#[derive(Debug, Default)]
pub struct Atom {
    pub start: u64,
    pub len: u64,
    pub name: SmallString<[u8; 4]>,
    pub children: Vec<Atom>,
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

    if let Ok(name) = from_utf8(&header[4..]) {
        ret.name = SmallString::from_str(name);
    } else {
        //TODO: custom error enum; also this may not be a valid assumption
        //return Err(Error::new(ErrorKind::InvalidData, "name was not utf-8"));

        //TODO: okay i found one with a non-utf8 name, now what?
        ret.name = SmallString::from_str("xxxx");
    }

    if CONTAINER_ATOMS.iter().any(|&cont| cont == ret.name) {
        if &*ret.name == "meta" {
            file.seek(SeekFrom::Current(4))?;
        } else if &*ret.name == "stsd" {
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
