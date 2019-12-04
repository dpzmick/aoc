use std::fs::File;
use std::io::Read;
use std::iter::Iterator;
use std::pin::Pin;

// attempt to read the file one "token" at a time by splitting on either whitespace or a newline
// hard to use split method here, the lifetime of the string is ambiguous
// so just implement a buffered reader
// complete overkill, just trying something new out with iterators (Pin thing, didn't work anyway...)
struct IterThing<R: Read> {
    rdr: R,
    buf: Box<[u8]>,
    idx: usize,
    len: usize,
}

impl<R: Read> IterThing<R> {
    fn new(rdr: R) -> Self {
        Self {
            rdr,
            buf: Box::new([0; 4096]),
            idx: 0,
            len: 0,
        }
    }
}

// doesn't work, the lifetime needs to be "until the next call to next()" which we can't express
// impl<'a, R: Read> Iterator for Pin<&'a mut IterThing<R>> { type Item = &'a str; }

impl<R: Read> Iterator for IterThing<R> {
    type Item = String;
    fn next(&mut self)-> Option<Self::Item> {
        if self.len == 0 {
            self.len = self.rdr.read(&mut self.buf).expect("IO error");
        }

        if self.len == 0 { return None; }

        let sep = |c| { c == b',' || c == b'\n' };

        while self.idx < self.len {
            if !sep(self.buf[self.idx]) { break; }
            self.idx += 1;
        }

        if self.idx == self.len { return None; }

        let mut end = self.idx;
        while end < self.len {
            end += 1;
            if sep(self.buf[end]) { break; }
        }

        let spl = &self.buf[self.idx..end];
        self.idx = end;
        Some( std::str::from_utf8(spl).expect("UTF-8 failure").into() )
    }
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let mut numbers: Vec<u32> = IterThing::new(file)
        .map(|l| l.parse::<u32>().expect("Bad input"))
        .collect();

    numbers[1] = 12;
    numbers[2] = 2;

    // assumes the input is well formed...
    let mut pc = 0;
    loop {
        println!("numbers: {:?}", numbers);
        let op = numbers[pc];
        if op == 99 { break }

        let (x, y, dst) = (numbers[pc+1] as usize, numbers[pc+2] as usize, numbers[pc+3] as usize);
        numbers[dst] = match op {
            1 => numbers[x] + numbers[y],
            2 => numbers[x] * numbers[y],
            _ => panic!("bad op"),
        };

        pc += 4;
    }
    println!("numbers: {:?}", numbers);
}
