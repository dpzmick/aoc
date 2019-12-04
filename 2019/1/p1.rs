use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn fuel_required(mass: u32) -> u32 {
    (mass / 3) - 2
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let reader = BufReader::new(file);
    let fuel: u32 = reader
        .lines()
        .map(|l| l.unwrap())
        .map(|l| l.parse::<u32>().expect("Bad input"))
        .map(fuel_required)
        .sum();

    println!("fuel: {}", fuel);
}
