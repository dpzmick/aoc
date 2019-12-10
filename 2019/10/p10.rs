use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::collections::HashSet;
use std::collections::HashMap;

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let reader = BufReader::new(file);
    let locs = reader.lines().map(|l| l.expect("bad io")).enumerate()
        .flat_map(|(y, l)| {
            l.chars().enumerate()
                .filter(|(_, c)| *c == '#')
                .map(|(x, _)| (x, y))
                .collect::<Vec<_>>() // collect required to copy that chars, the iterator is a borrow
        }).collect::<Vec<_>>();

    println!("locs: {:?}", locs);

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
    for ( (x1, y1), (x2, y2) ) in &s {
        let xdiff = *x1 as f64 - *x2 as f64;
        let ydiff = *y1 as f64 - *y2 as f64;
        let mag   = (xdiff.powf(2.) + ydiff.powf(2.)).sqrt();
        let angle = ydiff.atan2(xdiff);

        let mut blocker = false;
        for (b_x, b_y) in &locs {
            if b_x == x1 && b_y == y1 { continue; }
            if b_x == x2 && b_y == y2 { continue; }

            // if not closer than the points being compared, not blocking
            let xdiff = *x1 as f64 - *b_x as f64;
            let ydiff = *y1 as f64 - *b_y as f64;
            let bmag  = (xdiff.powf(2.) + ydiff.powf(2.)).sqrt();
            let bangle = ydiff.atan2(xdiff);

            if mag < bmag {continue;}

            if angle >= bangle && angle <= bangle {
                blocker = true;
            }

            // println!("{},{}->{},{} through {},{}: angle={}, block_angle={} blocked={}",
            //          x1, y1, x2, y2, b_x, b_y,
            //          angle*180./std::f64::consts::PI,
            //          bangle*180./std::f64::consts::PI,
            //          blocker);
        }

        if !blocker {
            *cnt.entry((x1, y1)).or_insert(0) += 1;
            *cnt.entry((x2, y2)).or_insert(0) += 1;
        }
    }

    for e in &cnt {
        println!("{:?}", e);
    }

    println!("{:?}", cnt.iter().max_by_key(|(_, v)| *v));
}
