// how much order needed to produce one unit of fuel

// reaction:
// - inputs: [(element, quantity)] -> (element, quantity)

use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::collections::VecDeque;

type Inventory = Vec<usize>;
type ChemicalID = usize;

// backwards
struct Reaction {
    input: (ChemicalID, usize),          // exaclty one, these are "backwards"
    outputs: Vec<(ChemicalID, usize)>,   // can create many
}

struct NameID {
    strings: Vec<String>,
}

impl NameID {
    fn new() -> Self {
        Self { strings: Vec::new() }
    }

    fn intern(&mut self, v: String) -> usize {
        self.strings.push(v);
        self.strings.len() - 1
    }

    fn get_by_id(&self, id: usize) -> Option<&String> {
        self.strings.get(id)
    }

    fn get_by_string(&self, v: &str) -> Option<usize> {
        for (i, s) in self.strings.iter().enumerate() {
            if s == v { return Some(i); }
        }

        return None;
    }

    fn max_name(&self) -> usize {
        self.strings.len() - 1
    }
}

fn solve(reactions: &[Reaction], target: usize, names: &NameID, distances: &[usize]) -> usize {
    let fuelid = names.get_by_string("FUEL").expect("no FUEL");
    let oreid  = names.get_by_string("ORE").expect("no ORE");

    let mut inventory = Vec::new();
    for _ in 0..=names.max_name() {
        inventory.push(0);
    }

    // lets go
    inventory[fuelid] = target;
    loop {
        // find the thing that is farthest from ore, figure out how to convert it
        let (chem, amount, _) = inventory.iter().enumerate()
            .filter(|(_, v)| **v != 0)
            .map(|(i, v)| (i, *v, distances[i]))
            .max_by_key(|(i, v, d)| *d)
            .unwrap();

        if chem == oreid { break; }

        // assume exactly one
        let reaction = reactions.iter()
            .filter(|r| r.input.0 == chem)
            .nth(0)
            .unwrap();

        // if we can't perform the reaction evenly, round
        let n = (amount as f64/ reaction.input.1 as f64).ceil() as usize;

        if n*reaction.input.1 <= inventory[reaction.input.0] {
            inventory[reaction.input.0] -= n*reaction.input.1;
        }
        else {
            inventory[reaction.input.0] = 0;
        }

        for output in reaction.outputs.iter() {
            inventory[output.0] += n*output.1;
        }
    }

    return inventory[oreid];
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let mut names = NameID::new();
    let reactions = BufReader::new(file).lines()
        .map(|l| l.unwrap())
        .map(|l| {
            let mut sides = l.split("=>");

            let convert = |l: &str| {
                let mut chunks = l.split(' ');
                let rhs = chunks.nth(0).unwrap();
                let lhs = chunks.nth(0).unwrap();
                (lhs.trim().to_string(), rhs.parse::<usize>().expect("bad number"))
            };

            let mut intern = |n: String| -> usize {
                match names.get_by_string(&n) {
                    Some(id) => id,
                    None     => names.intern(n),
                }
            };

            let rhs = sides.nth(0).unwrap().split(',').map(|l| l.trim()).map(convert).map(|(s,c)| (intern(s), c)).collect();
            let lhs = sides.nth(0).unwrap().split(',').map(|l| l.trim()).map(convert).map(|(s,c)| (intern(s), c)).nth(0).unwrap();

            // backwards
            Reaction {
                input: lhs,
                outputs: rhs,
            }
        })
        .collect::<Vec<Reaction>>();

    // this could be way better, but I'm tired of this problem
    let mut distances = Vec::new();
    for chem in 0..=names.max_name() {
        let mut q = VecDeque::new();
        q.push_back(chem);

        let mut dist = 0;
        while q.len() > 0 {
            let next = q.pop_front().unwrap();
            for reaction in reactions.iter() {
                if reaction.input.0 == next {
                    for output in reaction.outputs.iter() {
                        q.push_back(output.0);
                    }
                }
            }

            dist += 1;
        }

        distances.push(dist)
    }

    // there's good ways to do this, and bad ways to do this

    for target in 1..=1000000000000 {
        if target % 100000 == 0 { println!("target={}", target); }
        let ans = solve(&reactions, target, &names, &distances);
        if ans >= 1000000000000 {
            println!("target {} ans {}", target, ans);
            break;
        }
    }
}
