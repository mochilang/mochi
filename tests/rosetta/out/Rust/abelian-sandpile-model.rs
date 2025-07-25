// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fn main() {
    let dim = 16;
    fn newPile(d: i32) -> Vec<Vec<i32>> {
        let mut b: Vec<Vec<i32>> = vec![];
        let mut y = 0;
        while y < d {
            let mut row: Vec<i32> = vec![];
            let mut x = 0;
            while x < d {
                row = { let mut tmp = row.clone(); tmp.push(0); tmp };
                x += 1;
            }
            b = { let mut tmp = b.clone(); tmp.push(row); tmp };
            y += 1;
        }
        return b;
    }
    let handlePile = move |pile: &mut Vec<Vec<i32>>, x: i32, y: i32| -> Vec<Vec<i32>> {
        if pile[y as usize][x as usize] >= 4 {
            pile[y as usize][x as usize] = pile[y as usize][x as usize] - 4;
            if y > 0 {
                pile[y - 1 as usize][x as usize] = pile[y - 1 as usize][x as usize] + 1;
                if pile[y - 1 as usize][x as usize] >= 4 {
                    pile = handlePile(&mut pile, x, y - 1);
                }
            }
            if x > 0 {
                pile[y as usize][x - 1 as usize] = pile[y as usize][x - 1 as usize] + 1;
                if pile[y as usize][x - 1 as usize] >= 4 {
                    pile = handlePile(&mut pile, x - 1, y);
                }
            }
            if y < dim - 1 {
                pile[y + 1 as usize][x as usize] = pile[y + 1 as usize][x as usize] + 1;
                if pile[y + 1 as usize][x as usize] >= 4 {
                    pile = handlePile(&mut pile, x, y + 1);
                }
            }
            if x < dim - 1 {
                pile[y as usize][x + 1 as usize] = pile[y as usize][x + 1 as usize] + 1;
                if pile[y as usize][x + 1 as usize] >= 4 {
                    pile = handlePile(&mut pile, x + 1, y);
                }
            }
            pile = handlePile(&mut pile, x, y);
        }
        return pile;
    };
    fn drawPile(pile: Vec<Vec<i32>>, d: i32) -> () {
        let chars = vec![" ", "░", "▓", "█"];
        let mut row = 0;
        while row < d {
            let mut line = String::new();
            let mut col = 0;
            while col < d {
                let mut v = pile[row as usize][col as usize];
                if v > 3 {
                    v = 3;
                }
                line += chars[v as usize];
                col += 1;
            }
            println!("{}", vec![format!("{}", line)].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
            row += 1;
        }
    }
    fn main() -> () {
        let mut pile = newPile(16);
        let hdim = 7;
        pile[hdim as usize][hdim as usize] = 16;
        pile = handlePile(&mut pile, hdim, hdim);
        drawPile(pile, 16);
    }
    main();
}
