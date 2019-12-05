// don't think there's any way to do this smarter

// grid dimensions are not known ahead of time

use std::ops::Mul;
use std::fmt::Debug;

#[derive(Copy, Clone, Debug)]
enum Direction {
    H(i32),
    V(i32),
}

impl Direction {
    fn mag(&self) -> i32 {
        match self {
            Direction::H(v) => v.abs(),
            Direction::V(v) => v.abs(),
        }
    }

    fn unit(&self) -> Direction {
        match self {
            Direction::H(v) => Direction::H(v.signum()),
            Direction::V(v) => Direction::V(v.signum()),
        }
    }
}

// scaling operator
impl Mul<i32> for Direction {
    type Output = Self;
    fn mul(self, rhs: i32) -> Self {
        match self {
            Direction::H(x) => Direction::H(x * rhs),
            Direction::V(x) => Direction::V(x * rhs),
        }
    }
}

fn to_dir(inp: &str) -> Vec<Direction> {
    let f = |s: &str| {
        let unit_dir = match s.as_bytes()[0] {
            b'R' => Direction::H(1),
            b'L' => Direction::H(-1),
            b'U' => Direction::V(1),
            b'D' => Direction::V(-1),
            _   => panic!("Bad input"),
        };
        unit_dir * s[1..].parse::<i32>().expect("Bad Input")
    };

    inp.split(',').map(f).collect()
}

#[derive(Clone)]
struct LocIter<'a> {
    last:     (i32, i32),
    dir:      Direction,
    rem:      i32,
    dirs:     &'a [Direction]
}

impl<'a> LocIter<'a> {
    fn new(d: &'a [Direction]) -> Self {
        Self {
            last: (0, 0),
            dir:  d[0].unit(),
            rem:  d[0].mag(),
            dirs: &d[1..],
        }
    }
}

impl<'a> Iterator for LocIter<'a> {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.rem == 0 {
            if self.dirs.is_empty() { return None; }

            self.dir  = self.dirs[0].unit();
            self.rem  = self.dirs[0].mag();
            self.dirs = &self.dirs[1..]
        }

        let l = match self.dir {
            Direction::H(x) => (self.last.0 + x, self.last.1),
            Direction::V(y) => (self.last.0,     self.last.1 + y),
        };

        self.last = l;
        self.rem -= 1;
        Some(self.last)
    }
}

// struct Cart<I1: Iterator, I2: Iterator + Clone> {
//     a:     I1,
//     b:     I2,

//     ca:    Option<I1::Item>, // when this is done, we're done
//     fresh: I2,
// }

// impl<I1: Iterator, I2: Iterator + Clone> Cart<I1, I2> {
//     fn new(mut a: I1, b: I2) -> Self {
//         let ca    = a.next();
//         let fresh = b.clone();

//         Self {
//             a, b,
//             ca, fresh
//         }
//     }
// }

// impl<I1: Iterator, I2: Iterator + Clone> Iterator for Cart<I1, I2>
//     where <I1 as Iterator>::Item: Clone,
// {
//     type Item = (I1::Item, I2::Item);

//     fn next(&mut self) -> Option<Self::Item> {
//         let mut cb = self.b.next();
//         if cb.is_none() {
//             self.b = self.fresh.clone();
//             self.ca = self.a.next();
//             cb = self.b.next();
//         }

//         if self.ca.is_none() || cb.is_none() { return None; }
//         Some( (self.ca.clone().unwrap(), cb.unwrap()) )
//     }
// }

fn main() {
    let a = to_dir("R1003,D430,L108,D570,R459,U7,L68,D232,L130,U93,R238,U951,L821,U723,L370,U873,L680,U749,R110,U17,R185,U484,R550,U356,L212,U350,L239,D208,R666,U70,L369,U448,R54,D402,R165,D375,L468,U886,L303,U779,L752,U664,L120,U643,R405,D288,L220,U727,L764,D615,R630,U688,R961,U499,L782,D852,L743,U443,R355,U856,L795,U235,L876,D511,L108,U637,R427,D338,L699,D911,L506,D607,L539,U977,R654,D634,L196,U944,R922,D774,R358,U828,L970,D386,R795,U602,R249,U793,L171,D217,L476,D123,L179,U820,R895,D239,R363,D629,L226,D811,R962,D848,R218,D581,R369,D872,L653,D281,R304,D302,R780,U636,L413,D712,L642,D886,R613,U736,L968,D82,R953,U408,L130,U654,R312,U74,L610,D798,R242,D586,L808,D664,L764,U455,R264,U384,L154,D484,R883,D276,L423,U11,L145,U156,L268,U46,R202,U641,R920,D483,R859,U94,L173,D796,R11,D328,R48,D161,L615,D396,R350,D48,R946,D233,R385,D294,R640,D301,R810,D824,L969,D469,R34,U995,R750,D827,R52,U606,R143,U868,L973,U863,L17,U995,L236,U994,R403,D312,R49,U143,L399,U821,R974,U119,R410,D233,R228,D326,R112,D512,L950,D103,L590,U80,R7,U441,L744,U643,L80,D631,L576,U680,R369,U741,L87,D748,R773,U145,R464,U546,R80,D251,L972,U414,L390,U148,L84,D481,L425,U293,L564,U880,R535,U703,R981,U944,R224,D366,R29,U517,R342,U686,L384,D650,R983,D287,L108,U713,L523,U695,R881,D126,R151,U153,R161,D791,L599,D936,L816,U387,R411,U637,L434,D22,L720,U579,L661,D644,L220,U325,R753,D392,L503,U617,R1,D956,L607,U602,L800,D749,L193,U215,L91,U733,L606,U510,L124,D550,R303,D835,R19,D326,R577,U265,L156,D924,L122,D186,R803,U3,R879");
    let b = to_dir("L1003,U603,L675,U828,R671,U925,R466,D707,L39,U1,R686,U946,L438,U626,R714,D365,L336,D624,R673,U672,L729,D965,R824,D533,R513,D914,R829,U275,L424,U10,L244,U158,R779,D590,R116,U714,R662,D989,R869,D547,R817,U315,R439,D29,L599,D870,L645,U656,R845,U19,R960,U669,L632,D567,L340,U856,R955,D314,R452,D896,R574,D162,R240,U302,R668,U706,R394,D24,L422,U884,R804,U576,L802,U400,R405,U676,L344,D628,R672,U580,R710,U536,L712,U738,L266,D212,R552,D229,R265,U835,R152,U784,L478,D87,L783,D327,R728,U590,R408,D397,R363,D654,R501,D583,R445,U897,R888,D480,R455,U593,R906,D506,R985,D361,R361,D619,L462,D873,L248,D348,R540,D578,L274,D472,R254,U647,R54,U681,L33,U343,R913,U120,L64,D849,L953,U407,L64,U744,L482,U240,L82,U69,R480,D796,L137,U527,R428,U67,R123,U688,L985,D944,R583,D804,R331,U328,R906,U376,L966,U433,R863,D931,L315,D9,L77,D141,L738,D661,R742,D44,R383,U78,R106,D301,L186,U907,L304,U786,L256,U718,R861,D145,R694,D721,R607,D418,R358,U600,R228,D139,R476,D451,L49,U616,L491,U8,R371,D735,R669,U388,L905,D282,L430,U491,L775,U891,L831,U350,L247,D609,R489,U266,R468,D748,R134,U187,R882,D315,R344,D363,R349,U525,R831,U644,R207,D563,L1,D946,L559,U789,L187,U370,L284,D910,L394,D560,L705,U661,R272,U109,L12,D554,L670,D169,L375,D100,R382,D491,L53,D916,R152,U82,L236,U845,L860,U732,R327,D190,R888,U722,R770,U993,R509,D970,L225,D756,R444,D992,L746,D35,R329,D452,R728,U575,L325,U414,L709,D844,R692,D575,R132,D520,L506,D384,L581,U36,L336,U849,L944,U450,R138,D186,L613,U805,R32,U763,R210,U556,R125,D499,R729");
    // let a = to_dir("R75,D30,R83,U83,L12,D49,R71,U7,L72");
    // let b = to_dir("U62,R66,U55,R34,D71,R55,D58,R83");

    // let a = to_dir("R8,U5,L5,D3");
    // let b = to_dir("U7,R6,D4,L4");

    // let a: std::collections::HashSet<_> = LocIter::new(&a).collect();
    // let b: std::collections::HashSet<_> = LocIter::new(&b).collect();
    // let ans = a.intersection(&b).map(|(a, b)| a.abs() + b.abs()).min();
    // println!("{:?}", ans);

    // part2
    use std::collections::*;
    let f = |mut acc: HashMap<_, _>, (x, y)| if !acc.contains_key(&y) { acc.insert(y, x); acc } else { acc };
    let a = LocIter::new(&a).enumerate().map(|(a, b)| (a+1, b)).fold(HashMap::new(), f);
    let b = LocIter::new(&b).enumerate().map(|(a, b)| (a+1, b)).fold(HashMap::new(), f);
    let a = a.keys().collect::<HashSet<_>>().intersection(&b.keys().collect::<HashSet<_>>() )
        .map(|k| a[k] + b[k])
        .min()
        .unwrap();

    println!("{}", a);
}
