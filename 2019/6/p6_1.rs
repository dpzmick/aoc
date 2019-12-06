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

    let mut adj: Vec<Vec<_>>               = Vec::new();
    let mut nameid: HashMap<String, usize> = HashMap::new();
    let mut idname: HashMap<usize, String> = HashMap::new();
    for (a, b) in it {
        let n = nameid.len();
        let aid = *nameid.entry(a.clone()).or_insert(n);

        let n = nameid.len();
        let bid = *nameid.entry(b.clone()).or_insert(n);

        idname.entry(nameid[&a]).or_insert(a.to_string());
        idname.entry(nameid[&b]).or_insert(b.to_string());

        let extent = std::cmp::max(aid, bid);
        for i in 0..adj.len() {
            // add new column if needed
            for _ in adj[i].len()..(extent+1) {
                adj[i].push(false)
            }
        }

        // add new row if needed
        for i in adj.len()..(extent+1) {
            // entire column needs to be created
            let mut column = Vec::new();
            for _ in 0..(extent+1) { column.push(false); }
            adj.push(column);
        }

        adj[aid][bid] = true;
        adj[bid][aid] = true;
    }

    let mut visited = HashSet::new();
    let mut q = VecDeque::new();
    q.push_front((nameid["YOU"], 0));

    loop {
        let (nxt, dist) = q.pop_back().unwrap();
        if nxt == nameid["SAN"] {
            println!("{}", dist-2);
            break;
        }

        if visited.contains(&nxt) { continue; }
        println!("nxt: {}", idname[&nxt]);

        for (i, entry) in adj[nxt].iter().enumerate() {
            if *entry {
                q.push_front( (i, dist+1) );
            }
        }

        visited.insert(nxt);
    }
}
