use std::fs::File;
use std::io::Read;

// series of digits representing pixels
// fill left to right, then top to bottom

// image dimensions are an input

fn main() {
    let (width, height) = (25, 6);
    // let (width, height) = (3, 2);

    let mut file = File::open("input").expect("Failed to open input");

    // size of layer
    let mut b: Vec<u8> = Vec::new();
    b.resize_with(width*height, Default::default);

    let mut best: Option<[usize; 3]> = None;
    loop {
        let mut cnt = [0, 0, 0];

        if let Err(_) = file.read_exact(&mut b) { break; } // assume EOF

        println!("reading {} for layer. {} avail", width*height, b.len());
        for i in 0..(width*height) {
            if b[i] == 48 {
                cnt[0] += 1;
            }
            else if b[i] == 49 {
                cnt[1] += 1;
            }
            else if b[i] == 50 {
                cnt[2] += 1;
            }
        }

        if best.is_none() || best.unwrap()[0] > cnt[0] {
            best = Some(cnt);
        }
    }

    println!("best: {:?} ans: {}", best, best.unwrap()[1] * best.unwrap()[2]);
}
