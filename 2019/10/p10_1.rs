use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::collections::HashSet;
use std::collections::HashMap;

fn blocks(a: (usize, usize), b: (usize, usize), through: (usize, usize), h: u64) -> bool {
    let (x1, y1) = a;
    let (x2, y2) = b;
    let (bx, by) = through;

    let xdiff = x1 as f64 - x2 as f64;
    let ydiff = y1 as f64 - y2 as f64;
    let ab_magnitude = (xdiff.powf(2.) + ydiff.powf(2.)).sqrt();
    let ab_angle     = ydiff.atan2(xdiff);

    let xdiff = x1 as f64 - bx as f64;
    let ydiff = y1 as f64 - by as f64;
    let block_magnitude = (xdiff.powf(2.) + ydiff.powf(2.)).sqrt();
    let block_angle     = ydiff.atan2(xdiff);

    // problem is 33x33
    let min_angle = (1.0_f64/((h - 1) as f64)).atan()
        - (1.0_f64/(h as f64)).atan();
    // println!("min angle: {}", min_angle.to_degrees());

    let blocker = block_magnitude < ab_magnitude && (ab_angle - block_angle).abs() <= min_angle;
    // println!("{},{}->{},{} through {},{}: angle={}, block_angle={} blocked={}",
    //          x1, y1, x2, y2, bx, by,
    //          ab_angle,
    //          block_angle,
    //          blocker);
    blocker
}

fn print_grid(highlight1: Option<(usize, usize)>, highlight2: Option<(usize, usize)>, locs: &Vec<(usize, usize)>) {
    let w = locs.iter().map(|(x, y)| *x).max().unwrap()+1;
    let h = locs.iter().map(|(x, y)| *y).max().unwrap()+1;

    print!("   ");
    for x in 0..w {
        print!("{:<3}", x);
    }
    println!("");

    for y in 0..h {
        print!("{:<3}", y);
        for x in 0..w {
            if Some((x,y)) == highlight1 {
                print!("\x1b[1;32mS  \x1b[0m");
            }
            else if Some((x,y)) == highlight2 {
                print!("\x1b[1;31mX  \x1b[0m");
            }
            else if locs.contains(&(x, y)) {
                print!("#  ");
            }
            else {
                print!(".  ");
            }
        }
        println!();
    }
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let reader = BufReader::new(file);
    let mut lines = 0;
    let mut locs = reader.lines().map(|l| l.expect("bad io")).enumerate()
        .flat_map(|(y, l)| {
            lines += 1;
            l.chars().enumerate()
                .filter(|(_, c)| *c == '#')
                .map(|(x, _)| (x, y))
                .collect::<Vec<_>>() // collect required to copy that chars, the iterator is a borrow
        }).collect::<Vec<(usize, usize)>>();

    let h = lines+1;
    // let h = 17;
    println!("h: {}", h);

    // print_grid(None, None, &locs);

    let mut s = HashSet::new();
    for a in &locs {
        for b in &locs {
            if a == b { continue; }
            if !s.contains( &(a, b) ) && !s.contains( &(b, a) ) {
                s.insert( (a, b) );
            }
        }
    }

    let mut cnt = HashMap::new();
    for (a, b) in &s {
        let blocked = locs.iter().any(|through| blocks(**a, **b, *through, h));

        if !blocked {
            *cnt.entry(a).or_insert(0) += 1;
            *cnt.entry(b).or_insert(0) += 1;
        }
    }

    let (s_x, s_y) = cnt.iter().max_by_key(|(_, v)| *v).unwrap().0;
    let station = (*s_x, *s_y);
    std::mem::drop(cnt);

    // println!("station at {},{}", s_x, s_y);

    let mut i = 1;
    let mut angle = 3.*std::f64::consts::PI/2.-0.001; // (0, 2pi]
    loop {
        if locs.len() == 1 { break; } // only self
        // println!("{}: angle={}", i, angle.to_degrees());

        // find everything that we can actually get to
        let selection = locs.iter()
            .filter(|b| **b != station) // can't shoot yourself
            .filter(|b| {
                !locs.iter()
                    .filter(|through| **through != station)
                    .filter(|through| **through  != **b)
                    .any(|through| blocks(station, **b, *through, h))
            })
            .flat_map(|b| {
                let twopi = 2.*std::f64::consts::PI;
                let b_angle = (b.1 as f64 - station.1 as f64).atan2(b.0 as f64 - station.0 as f64); // (-pi, pi]
                let b_angle_clockwise = if b_angle < 0. { b_angle + twopi } else { b_angle };       // (0, 2pi]
                // println!("{:?} {} {}", b, b_angle.to_degrees(), b_angle_clockwise.to_degrees());
                assert!(b_angle > -std::f64::consts::PI && b_angle <= std::f64::consts::PI);

                let b = *b;
                (0..2).map(move |pis| (b, b_angle_clockwise + (pis as f64)*twopi))
            })
            .filter(|(b, b_angle)| *b_angle > angle)
            .map(|(b, b_angle)| (b, b_angle - angle))
            .fold(None, |acc: Option<((usize, usize), f64)>, e| {
                if acc.is_none() || e.1 <= acc.unwrap().1 {
                    Some(e)
                }
                else {
                    acc
                }
            }).expect("unexpected");

        let idx = locs.iter().enumerate().filter(|(i, b)| **b == selection.0).map(|(i, _)| i).nth(0).unwrap();
        locs.remove(idx);

        println!("{}: {:?} angle={} angle_to_asteroid={}",
                 i, selection.0, angle.to_degrees(), (selection.1).to_degrees());
        print_grid(Some(station), Some(selection.0), &locs);
        println!("\n");

        // figure out the absolute angle again, just recompute it because why not
        let b = selection.0;
        let twopi = 2.*std::f64::consts::PI;
        let b_angle = (b.1 as f64 - station.1 as f64).atan2(b.0 as f64 - station.0 as f64); // (-pi, pi]
        let b_angle_clockwise = if b_angle < 0. { b_angle + twopi } else { b_angle };       // (0, 2pi]

        let min_angle = (1.0_f64/((h - 1) as f64)).atan()
            - (1.0_f64/(h as f64)).atan();
        angle = b_angle_clockwise + min_angle; // keep going forward
        i += 1;
    }
}
