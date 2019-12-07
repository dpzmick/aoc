// configure a series of amplifiers
// start 12:46am

// 5 installed.
// amplifier control program is input
// ugh this sucks

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

fn eval(program: &mut [i32], input: &[i32]) -> i32 {
    let mut input_iter = input.iter();
    let mut pc = 0;
    loop {
        let op = program[pc];
        // if op == 99 { break } // FIXME can this still happen?

        // every value we care about is in numbers somewhere, but not contiguous
        let param_spec = (op/100) as usize;
        let pidx = |idx| {
            let spec = (if idx == 0 { param_spec } else { (param_spec/(10*idx)) })%10;
            match spec {
                0 => program[pc+idx+1] as usize,
                1 => pc+idx+1,
                _ => panic!("bad spec"),
            }
        };

        let op = op%100;
        pc = match op {
            1 => { let idx = pidx(2); program[idx] = program[pidx(0)] + program[pidx(1)]; pc + 4 },
            2 => { let idx = pidx(2); program[idx] = program[pidx(0)] * program[pidx(1)]; pc + 4 },
            3 => { let idx = pidx(0); program[idx] = *(input_iter.next().unwrap());       pc + 2 },
            4 => return program[pidx(0)],
            5 => if program[pidx(0)] > 0  { program[pidx(1)] as usize } else { pc + 3 },
            6 => if program[pidx(0)] == 0 { program[pidx(1)] as usize } else { pc + 3 },
            7 => { let idx = pidx(2); program[idx] = if program[pidx(0)] < program[pidx(1)] { 1 } else { 0 };  pc + 4 },
            8 => { let idx = pidx(2); program[idx] = if program[pidx(0)] == program[pidx(1)] { 1 } else { 0 }; pc + 4 },
            _ => panic!("bad op"),
        };
    }
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let program: Vec<i32> = IterThing::new(file)
        .map(|l| l.parse::<i32>().expect("Bad input"))
        .collect();

    let mut phases = (0..5).collect::<Vec<_>>();
    let mut swiz  = vec![0; phases.len()];
    let mut perms = Vec::new(); // lazy
    perms.push( phases.clone() );

    // FIXME actually understand this
    let mut i = 0;
    while i < phases.len() {
        if swiz[i] < i {
            if i & 1 == 0 {
                phases.swap(0, i);
            }
            else {
                phases.swap(swiz[i], i);
            }
            perms.push( phases.clone() );
            swiz[i] += 1;
            i = 0;
        }
        else {
            swiz[i] = 0;
            i += 1;
        }
    }

    use std::collections::HashSet;
    println!("n: {}", perms.iter().collect::<HashSet<_>>().len()); // should be 120

    let res = perms.iter().map(|perm| {
        // feels like there should be an adapter for this
        let mut prev = 0;
        for i in 0..5 {
            let inputs = [perm[i], prev];
            prev = eval(&mut program.clone(), &inputs);
        }
        prev
    })
    .max();

    println!("res: {:?}", res);
}
