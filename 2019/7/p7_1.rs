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

fn eval_one(program: &mut [i64], input: &[i64], mut pc: usize) -> Option<(i64, usize)> {
    let mut input_iter = input.iter();
    loop {
        let op = program[pc];
        if op == 99 { return None; }

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
            4 => return Some( (program[pidx(0)], pc + 2) ),
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
    let program: Vec<i64> = IterThing::new(file)
        .map(|l| l.parse::<i64>().expect("Bad input"))
        .collect();

    let mut phases = (5..10).collect::<Vec<_>>();
    let mut swiz  = vec![0; phases.len()];
    let mut perms = Vec::new(); // lazy
    perms.push( phases.clone() );

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

    let mut max: Option<(_,_)> = None;
    // for perm in &[ &[9,8,7,6,5] ]{
    for perm in perms {
        let mut machines = std::iter::repeat( program.clone() ).take( perm.len() ).collect::<Vec<_>>();
        let mut idx      = std::iter::repeat( 0 ).take( perm.len() ).collect::<Vec<_>>();

        let mut first = true;
        let mut prev = 0;
        loop {
            let mut done = false;
            for (i, (machine, idx)) in machines.iter_mut().zip(idx.iter_mut()).enumerate() {
                let inputs = if first { vec![perm[i], prev] } else { vec![prev] };
                match eval_one(machine, &inputs, *idx) {
                    Some( (p, pc) ) => {
                        prev = p;
                        *idx = pc;
                    }
                    None => done = true, // finish the for loop
                }
            }
            first = false;
            if done { break; }
        }

        if max.is_none() || max.clone().unwrap().0 < prev { max = Some((prev, perm.clone())); }
    }

    println!("{:?}", max);
}
