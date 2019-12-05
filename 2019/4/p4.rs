// all in branch free

fn main() {
    let mut cnt = 0;
    for i in 147981..(691423+1) {
    // for i in 0..999999 {
    // for i in vec![599999] {
    // for i in vec![147999] {
    // for i in vec![234568] {
    // for i in vec![667788] {
        let mut ii = i;
        let mut last = ii%10;
        ii /= 10;

        let mut run_len    = 1;
        let mut increasing = true;
        let mut min_run    = 100;

        while ii != 0 {
            let dig = ii%10;

            let cond = (last != dig) && (run_len > 1);
            min_run    = (cond as i16)*std::cmp::min(min_run, run_len)
                + (!cond as i16)*min_run;
            run_len    = ((last == dig) as i16)*run_len + 1;
            increasing = increasing && last >= dig;

            last = dig;
            ii /= 10;
        }

        if run_len > 1 {
            min_run = std::cmp::min(run_len, min_run);
        }

        // 666 7 89
        // 5 99999

        // if !increasing || min_run == 100 {
        //     continue;
        // }

        // part 2
        if !increasing || min_run != 2 { continue; }
        cnt += 1;
    }

    println!("{}", cnt);
}
