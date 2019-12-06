use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

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
    for (a, b) in it { // only read file once, at the cost of doing tons of allocations
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

        for (i, entry) in adj[nxt].iter().enumerate() {
            if *entry {
                q.push_front( (i, dist+1) );
            }
        }

        visited.insert(nxt);
    }
}


/* irritating that this code is fast:
$ /usr/bin/time -v ./p6_1
298
	Command being timed: "./p6_1"
	User time (seconds): 0.00
	System time (seconds): 0.00
	Percent of CPU this job got: 90%
	Elapsed (wall clock) time (h:mm:ss or m:ss): 0:00.01
	Average shared text size (kbytes): 0
	Average unshared data size (kbytes): 0
	Average stack size (kbytes): 0
	Average total size (kbytes): 0
	Maximum resident set size (kbytes): 4528
	Average resident set size (kbytes): 0
	Major (requiring I/O) page faults: 0
	Minor (reclaiming a frame) page faults: 769
	Voluntary context switches: 1
	Involuntary context switches: 0
	Swaps: 0
	File system inputs: 0
	File system outputs: 0
	Socket messages sent: 0
	Socket messages received: 0
	Signals delivered: 0
	Page size (bytes): 4096
	Exit status: 0 */
