use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

#[derive(Debug)]
struct Moon {
    location: Point,
    velocity: Point,
}

// on each step
// - update velocity (+1 or -1 only), one velocity for each direction. Do for all pairs.
// - apply velocity (move each moon by their velocity vector)

// can I do this smarter? perhaps not a good use of my time to do it smarter

fn det_vel(ca: i64, cb: i64) -> i64 /* delta for a */ {
    match ca.cmp(&cb) {
        std::cmp::Ordering::Greater => -1,
        std::cmp::Ordering::Equal   =>  0,
        std::cmp::Ordering::Less    =>  1,
    }
}

fn energy(moons: &Vec<Moon>) -> i64 {
    moons.iter()
        .map(|moon| {
            let potential = moon.location.x.abs() + moon.location.y.abs() + moon.location.z.abs();
            let kinetic = moon.velocity.x.abs() + moon.velocity.y.abs() + moon.velocity.z.abs();
            potential * kinetic
        })
        .sum()
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let reader = BufReader::new(file);
    let mut moons = reader.lines()
        .map(|l| l.expect("IO error"))
        .map(|l| {
            let mut sl = l.split(|c| *(&[' ', '<', '>', '=', ','].contains(&c)));
            let point = Point {
                x: sl.nth(2).expect("nth").parse::<i64>().expect("bad int"),
                y: sl.nth(2).expect("nth").parse::<i64>().expect("bad int"),
                z: sl.nth(2).expect("nth").parse::<i64>().expect("bad int"),
            };

            Moon {
                location: point,
                velocity: Point { x: 0, y: 0, z: 0 },
            }
        })
        .collect::<Vec<_>>();

    let n = 1000;
    for step in 0..=n {
        println!("After {} step (energy {}):", step, energy(&moons));
        for i in 0..moons.len() {
            println!("pos=<x={:4}, y={:4}, z={:4}> vel=<x={:4}, y={:4}, z={:4}>",
                     moons[i].location.x, moons[i].location.y, moons[i].location.z,
                     moons[i].velocity.x, moons[i].velocity.y, moons[i].velocity.z);
        }
        println!("");

        for i in 0..moons.len() {
            for j in 0..moons.len() {
                if i == j { continue; }

                // all pairs, need both directions so just iterate twice
                moons[i].velocity.x += det_vel(moons[i].location.x, moons[j].location.x);
                moons[i].velocity.y += det_vel(moons[i].location.y, moons[j].location.y);
                moons[i].velocity.z += det_vel(moons[i].location.z, moons[j].location.z);
            }
        }

        for i in 0..moons.len() {
            moons[i].location.x += moons[i].velocity.x;
            moons[i].location.y += moons[i].velocity.y;
            moons[i].location.z += moons[i].velocity.z;
        }
    }
}
