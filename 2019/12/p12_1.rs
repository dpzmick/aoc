use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug, Clone, PartialEq)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

#[derive(Debug, Clone, PartialEq)]
struct Moon {
    location: Point,
    velocity: Point,
}

fn gcd(mut a: i64, mut b: i64) -> i64 {
    while b > 0 {
        let t = b;
        b = a % b;
        a = t;
    }

    return a;
}

fn lcm(a: i64, b: i64) -> i64 {
    (a / gcd(a, b)) * b
}

// on each step
// - update velocity (+1 or -1 only), one velocity for each direction. Do for all pairs.
// - apply velocity (move each moon by their velocity vector)

// - need to be smarter than this, too slow
// - looking for cycles, any properties that can be taken advantage of?

// - each axis is independent, the location on the x axis has nothing
//   to do with the location on the y axis. This is super
//   counterintutive to me.

fn det_vel(ca: i64, cb: i64) -> i64 /* delta for a */ {
    match ca.cmp(&cb) {
        std::cmp::Ordering::Greater => -1,
        std::cmp::Ordering::Equal   =>  0,
        std::cmp::Ordering::Less    =>  1,
    }
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

    let init = moons.clone();

    // we care about how long it takes to reach init.x, init.y, and init.z
    let mut axis_cycle = [None; 3];

    let mut step = 0;
    loop {
        if step > 0 {
            // check x
            if axis_cycle[0].is_none() {
                let cycle = moons.iter()
                    .enumerate()
                    .filter(|(i, m)| m.location.x == init[*i].location.x)
                    .filter(|(i, m)| m.velocity.x == init[*i].velocity.x)
                    .count() == moons.len();

                if cycle {
                    axis_cycle[0] = Some(step);
                }
            }

            // check y
            if axis_cycle[1].is_none() {
                let cycle = moons.iter()
                    .enumerate()
                    .filter(|(i, m)| m.location.y == init[*i].location.y)
                    .filter(|(i, m)| m.velocity.y == init[*i].velocity.y)
                    .count() == moons.len();

                if cycle {
                    axis_cycle[1] = Some(step);
                }
            }

            // check z
            if axis_cycle[2].is_none() {
                let cycle = moons.iter()
                    .enumerate()
                    .filter(|(i, m)| m.location.z == init[*i].location.z)
                    .filter(|(i, m)| m.velocity.z == init[*i].velocity.z)
                    .count() == moons.len();

                if cycle {
                    axis_cycle[2] = Some(step);
                }
            }
        }

        if axis_cycle.iter().all(|e| e.is_some()) { break; }

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

        step += 1;
    }

    println!("cycles: {:?}", axis_cycle);

    // find lcm of these
    println!("ans {}", lcm(axis_cycle[0].unwrap(), lcm(axis_cycle[1].unwrap(), axis_cycle[2].unwrap())));
}
