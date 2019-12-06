use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

fn visit(name: &str, map: &HashMap<String, HashSet<String>>, depth: u64) -> u64 {
    let mut ret = 0; // triangle number
    for i in 0..depth {
        ret += 1;
    }

    if !map.contains_key(name) {
        return ret;
    }

    for child in &map[name] {
        ret += visit(child, map, depth+1);
    }

    return ret;
}

fn main() {
    let file = File::open("input").expect("Failed to open input");
    let it = BufReader::new(file)
        .lines()
        .map(|l| l.unwrap())
        .map(|l| l.trim().to_string())  //.split(")"))
        .map(|l| {
            let mut parts = l.split(")");
            (parts.next().unwrap().to_string(), parts.next().unwrap().to_string())
        });

    let mut map = HashMap::new();
    for (a, b) in it {
        map.entry(a).or_insert(HashSet::new()).insert(b);
    }

    println!("{:#?}", visit("COM", &map, 0));
}
