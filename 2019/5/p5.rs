#![feature(nll)]

use std::fs::File;
use std::io::Read;
use std::iter::Iterator;

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
    let mut numbers: Vec<i32> = IterThing::new(file)
        .map(|l| l.parse::<i32>().expect("Bad input"))
        .collect();

    let input = vec![1];
    let mut input_iter = input.iter();

    // assumes the input is well formed...
    let mut pc = 0;
    loop {
        let op = numbers[pc];
        if op == 99 { break }
        // println!("numbers: {:?}", numbers);

        // every value we care about is in numbers somewhere, but not contiguous
        let param_spec = (op/100) as usize;
        let pidx = |idx| {
            let spec = (if idx == 0 { param_spec } else { (param_spec/(10*idx)) })%10;
            match spec {
                0 => numbers[pc+idx+1] as usize,
                1 => pc+idx+1,
                _ => panic!("bad spec"),
            }
        };

        let op = op%100;
        match op {
            1 => { let idx = pidx(2); numbers[idx] = numbers[pidx(0)] + numbers[pidx(1)]; },
            2 => { let idx = pidx(2); numbers[idx] = numbers[pidx(0)] * numbers[pidx(1)]; },
            3 => { let idx = pidx(0); numbers[idx] = *(input_iter.next().unwrap()); },
            4 => println!("output: {}", numbers[pidx(0)]),
            _ => panic!("bad op"),
        };

        pc += match op {
            1 => 4,
            2 => 4,
            3 => 2,
            4 => 2,
            _ => panic!("bad op"),
        }
    }
}
