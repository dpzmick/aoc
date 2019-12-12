use std::fs::File;
use std::io::Read;
use std::iter::Iterator;
use std::collections::VecDeque;
use std::collections::HashMap;

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

struct Machine {
    pc: usize,
    rb: i64,
    memory: Vec<i64>,
    inputs: VecDeque<i64>,
}

impl Machine {
    fn new(program: Vec<i64>) -> Machine {
        Machine {
            pc: 0,
            rb: 0,
            memory: program,
            inputs: VecDeque::new(),
        }
    }

    fn input(&mut self, i: i64) {
        self.inputs.push_back(i)
    }

    fn run(&mut self) -> Option<i64> {
        loop {
            let op = self.memory[self.pc];
            if op == 99 { return None; }

            let param_spec = (op/100) as usize;
            let pidx = |idx: usize| {
                let spec = (if idx == 0 { param_spec } else { (param_spec/(10_usize.pow(idx as u32))) })%10;
                let ret = match spec {
                    0 => self.memory[self.pc+idx+1] as usize,
                    1 => self.pc+idx+1,
                    2 => (self.rb + self.memory[self.pc+idx+1]) as usize,
                    _ => panic!("bad spec"),
                };

                ret
            };

            let op = op%100;
            let mut ret = None;
            self.pc = match op {
                1 => {
                    let idx = pidx(2);
                    self.memory[idx] = self.memory[pidx(0)] + self.memory[pidx(1)];
                    self.pc + 4
                },
                2 => {
                    let idx = pidx(2);
                    self.memory[idx] = self.memory[pidx(0)] * self.memory[pidx(1)];
                    self.pc + 4
                },
                3 => {
                    let idx = pidx(0);
                    self.memory[idx] = self.inputs.pop_front().expect("input required");
                    self.pc + 2
                },
                4 => {
                    let idx = pidx(0);
                    ret = Some(self.memory[idx]);
                    self.pc + 2
                },
                5 => {
                    let idx = pidx(0);
                    if self.memory[idx] > 0 {
                        self.memory[pidx(1)] as usize
                    }
                    else {
                        self.pc + 3
                    }
                },
                6 => {
                    let idx = pidx(0);
                    if self.memory[idx] == 0 {
                        self.memory[pidx(1)] as usize
                    }
                    else {
                        self.pc + 3
                    }
                },
                7 => {
                    let idx = pidx(2);
                    self.memory[idx] = if self.memory[pidx(0)] < self.memory[pidx(1)] { 1 } else { 0 };
                    self.pc + 4
                },
                8 => {
                    let idx = pidx(2);
                    self.memory[idx] = if self.memory[pidx(0)] == self.memory[pidx(1)] { 1 } else { 0 };
                    self.pc + 4
                },
                9 => {
                    let idx = pidx(0);
                    self.rb += self.memory[idx];
                    self.pc + 2
                },
                _ => panic!("bad op"),
            };

            if ret.is_some() { return ret; }
        }
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

    let mut location = (0, 0);
    let mut m = Machine::new(program);
    let mut cells = HashMap::new();
    let mut angle = 90;
    loop {
        // println!("cells: {:?}", cells);
        // println!("loc {:?}", location);
        // let input = *cells.entry(location).or_insert(0); // part 1
        let input = *cells.entry(location).or_insert(1); // part 2
        m.input(input);
        match m.run() {
            Some(color) => *cells.get_mut(&location).unwrap() = color,
            None => break,
        };

        angle = match m.run() {
            Some(dir) => match dir {
                0 => angle + 90,
                1 => angle - 90,
                _ => panic!("bad"),
            },
            None => panic!("bad"),
        };

        while angle < 0 {
            angle += 360;
        }

        while angle >= 360 {
            angle -= 360;
        }

        location = match angle {
            0   => (location.0 + 1, location.1),
            90  => (location.0,     location.1 + 1),
            180 => (location.0 - 1, location.1),
            270 => (location.0,     location.1 - 1),
            _   => panic!("bad angle"),
        }
    }

    let w_mi = *cells.keys().map(|(x, _)| x).min().unwrap();
    let w_mx = *cells.keys().map(|(x, _)| x).max().unwrap();

    let h_mi = *cells.keys().map(|(_, y)| y).min().unwrap();
    let h_mx = *cells.keys().map(|(_, y)| y).max().unwrap();

    for y in (h_mi..(h_mx+1)).rev() {
        for x in w_mi..(w_mx+1) {
            let color = match cells.get( &(x,y) ) {
                Some(color) => if *color == 0 { ' ' } else { '#' },
                None        => '#',
            };
            print!("{}", color);
        }
        println!("");
    }
}
