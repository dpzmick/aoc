use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

// the equation double-checker didn't like my fuel calculation...
// fuel requires more fuel!

struct FuelIter { // is this interesting?
    next_fuel_mass: i32
}

impl FuelIter {
    fn reqd(mass: i32) -> i32 { (mass / 3) - 2 }

    fn new(module_mass: i32) -> Self {
        Self { next_fuel_mass: Self::reqd(module_mass) }
    }
}

impl Iterator for FuelIter {
    type Item = i32;

    fn next(&mut self) -> Option<i32> {
        if self.next_fuel_mass <= 0 {
            None
        }
        else {
            let last_fuel_mass = self.next_fuel_mass;
            self.next_fuel_mass = Self::reqd(last_fuel_mass);
            Some(last_fuel_mass)
        }
    }
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let reader = BufReader::new(file);

    // trying to figure out how to flatten this entire thing into a
    // single sum() call but then you need to do a chain over a
    // variable number of chains, or something... not sure that makes
    // any sense
    let fuel: i32 = reader
        .lines()
        .map(|l| l.unwrap())
        .map(|l| l.parse::<i32>().expect("Bad input"))
        .map(|module_mass| FuelIter::new(module_mass).sum::<i32>())
        .sum();

    println!("fuel: {}", fuel);
}
