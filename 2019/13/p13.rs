use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::iter::Iterator;
use std::collections::VecDeque;
use std::collections::HashMap;

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

fn fst<T: Copy>(t: &(T, T)) -> T { t.0 }
fn snd<T: Copy>(t: &(T, T)) -> T { t.1 }

fn draw(grid: &HashMap<(i64, i64), i64>) {
    let min_x = grid.keys().map(fst).min().unwrap();
    let max_x = grid.keys().map(fst).max().unwrap();

    let min_y = grid.keys().map(snd).min().unwrap();
    let max_y = grid.keys().map(snd).max().unwrap();

    // not sure what direction to draw in?
    println!("grid");
    for y in (min_y)..=(max_y) {
        for x in (min_y)..=(max_x) {
            let k = (x, y);
            if grid.contains_key(&k) {
                let v = grid[&k];
                if v == 0 {
                    print!(" ");
                }
                else {
                    print!("{}", v);
                }
            }
            else {
                print!(" ");
            }
        }
        println!("");
    }
    println!("\n\n");
}

#[derive(PartialEq, Eq)]
enum State {
    Init,     // waiting to our first move
    Wait,     // Waiting for the ball to move
    FireAway, // need to send a move
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let mut program: Vec<i64> = BufReader::new(file).lines()
        .map(|l| l.unwrap())
        .flat_map(|l| {
            l.split(',').map(|i| i.parse::<i64>().expect("bad input")).collect::<Vec<_>>()
        })
        .collect();

    for _ in 0..10000 { // jam crap on the end
        program.push(0);
    }

    program[0] = 2;

    let mut m                             = Machine::new(program);
    let mut grid                          = HashMap::new();
    let mut state                         = State::Init;
    loop {
        let ready = match (m.run(), m.run(), m.run()) {
            (Some(-1), Some(0), Some(sc)) => {
                println!("score: {}", sc);
                if state == State::Init {
                    state = State::FireAway;
                }

                true
            },
            (Some(x), Some(y), Some(id)) => {
                if state == State::Wait && id == 4 {
                    state = State::FireAway;
                }
                *grid.entry((x,y)).or_insert(0) = id;
                false
            },

            (None, _, _) => break,
            _            => panic!("bad"),
        };

        if state == State::FireAway {
            let ball_loc = grid.iter()
                .filter(|(loc, id)| **id == 4)
                .nth(0)
                .map(|(loc, _)| *loc)
                .unwrap();

            let pad_loc = grid.iter()
                .filter(|(loc, id)| **id == 3)
                .nth(0)
                .map(|(loc, _)| *loc)
                .unwrap();

            if ball_loc.0 < pad_loc.0 {
                m.input(-1);
            }
            else if ball_loc.0 > pad_loc.0 {
                m.input(1);
            }
            else {
                m.input(0);
            }

            state = State::Wait;
            draw(&grid);
        }
    }
}
