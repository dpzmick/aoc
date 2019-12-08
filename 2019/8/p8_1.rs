use std::fs::File;
use std::io::Read;

// series of digits representing pixels
// fill left to right, then top to bottom

// image dimensions are an input

fn main() {
    let (width, height) = (25, 6);
    // let (width, height) = (2, 2);

    let mut file = File::open("input").expect("Failed to open input");

    // size of layer
    let mut b: Vec<u8> = Vec::new();
    b.resize_with(width*height, Default::default);

    let mut layers = Vec::new();
    loop {
        if let Err(_) = file.read_exact(&mut b) { break; } // assume EOF

        println!("reading {} for layer. {} avail", width*height, b.len());
        let mut layer = Vec::new();
        for i in 0..(width*height) {
            if b[i] == 48 {
                layer.push(0);
            }
            else if b[i] == 49 {
                layer.push(1);
            }
            else if b[i] == 50 {
                layer.push(2);
            }
            else {
                panic!("bad input");
            }
        }

        layers.push(layer);
    }

    let mut result = Vec::new();
    result.resize_with(width*height, Default::default);

    for (i, pos) in result.iter_mut().enumerate() {
        for layer in &layers {
            if layer[i] != 2 {
                *pos = layer[i];
                break;
            }
        }
    }

    let mut it = result.iter();
    for _ in 0..height {
        for _ in 0..width {
            let v = it.next().unwrap();
            if *v == 0 {
                print!(" ");
            }
            else {
                print!("#");
            }
        }
        println!("");
    }
}
