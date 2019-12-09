// need relative mode paramaters

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

fn eval_one(program: &mut [i64], input: &[i64]) -> Vec<i64> {
    let mut pc         = 0;
    let mut input_iter = input.iter();
    let mut out        = Vec::new();
    let mut rb: i64    = 0;

    loop {
        let op = program[pc];
        if op == 99 { return out; }

        // every value we care about is in numbers somewhere, but not contiguous
        let param_spec = (op/100) as usize;
        let pidx = |idx: usize| {
            let spec = (if idx == 0 { param_spec } else { (param_spec/(10_usize.pow(idx as u32))) })%10;
            // println!("spec {}", spec);
            let ret = match spec {
                0 => program[pc+idx+1] as usize,
                1 => pc+idx+1,
                2 => (rb + program[pc+idx+1]) as usize,
                _ => panic!("bad spec"),
            };

            // match spec {
            //     0 => println!("lookup at {} val {}", pc+idx+1, program[pc+idx+1]),
            //     1 => println!("immediate of {}", program[pc+idx+1]),
            //     2 => println!("compute rb {} pc {} ret {}", rb, program[pc+idx+1], ret),
            //     _ => (),
            // };

            ret
        };

        let op = op%100;
        // println!("op {} prog {:?} rb {}", op, &program[pc..pc+4], rb);
        pc = match op {
            1 => { program[pidx(2)] = program[pidx(0)] + program[pidx(1)]; pc + 4 },
            2 => { program[pidx(2)] = program[pidx(0)] * program[pidx(1)]; pc + 4 },
            3 => { program[pidx(0)] = *(input_iter.next().unwrap());       pc + 2 },
            4 => { out.push(program[pidx(0)]);                             pc + 2 },
            5 => if program[pidx(0)] > 0  { program[pidx(1)] as usize } else { pc + 3 },
            6 => if program[pidx(0)] == 0 { program[pidx(1)] as usize } else { pc + 3 },
            7 => { program[pidx(2)] = if program[pidx(0)] < program[pidx(1)] { 1 } else { 0 };  pc + 4 },
            8 => { program[pidx(2)] = if program[pidx(0)] == program[pidx(1)] { 1 } else { 0 }; pc + 4 },
            9 => { rb += program[pidx(0)]; pc + 2 },
            _ => panic!("bad op"),
        };
    }
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let mut program: Vec<i64> = IterThing::new(file)
        .map(|l| l.parse::<i64>().expect("Bad input"))
        .collect();

    for _ in 0..10000 { // jam crap on the end
        program.push(0);
    }

    let inputs = [2];
    let res = eval_one(&mut program, &inputs);
    println!("{:?}", res);
}
