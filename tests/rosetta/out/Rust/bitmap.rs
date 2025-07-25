// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Pixel {
        R: i32,
        G: i32,
        B: i32,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
struct Bitmap {
        cols: i32,
        rows: i32,
        px: Vec<Vec<Pixel>>,
}

fn main() {
    fn pixelFromRgb(c: i32) -> Pixel {
        let r = (((c as f64) / (65536 as f64)).parse::<i32>().unwrap()) % 256;
        let g = (((c as f64) / (256 as f64)).parse::<i32>().unwrap()) % 256;
        let b = c % 256;
        return Pixel { R: r, G: g, B: b };
    }
    fn rgbFromPixel(p: Pixel) -> i32 {
        return p.R * 65536 + p.G * 256 + p.B;
    }
    fn NewBitmap(x: i32, y: i32) -> Bitmap {
        let mut data: Vec<Vec<Pixel>> = vec![];
        let mut row = 0;
        while row < y {
            let mut r: Vec<Pixel> = vec![];
            let mut col = 0;
            while col < x {
                r = { let mut tmp = r.clone(); tmp.push(Pixel { R: 0, G: 0, B: 0 }); tmp };
                col += 1;
            }
            data = { let mut tmp = data.clone(); tmp.push(r); tmp };
            row += 1;
        }
        return Bitmap { cols: x, rows: y, px: data };
    }
    fn Extent(b: Bitmap) -> std::collections::HashMap<&'static str, i32> {
        return { let mut m = std::collections::BTreeMap::new(); m.insert("cols", b.cols); m.insert("rows", b.rows); m };
    }
    fn Fill(b: &mut Bitmap, p: Pixel) -> () {
        let mut y = 0;
        while y < b.rows {
            let mut x = 0;
            while x < b.cols {
                let mut px = b.px;
                let mut row = px[y as usize];
                row[x as usize] = p.clone();
                px[y as usize] = row;
                b.px = px;
                x += 1;
            }
            y += 1;
        }
    }
    fn FillRgb(b: Bitmap, c: i32) -> () {
        Fill(&mut b.clone(), &pixelFromRgb(c));
    }
    fn SetPx(b: &mut Bitmap, x: i32, y: i32, p: Pixel) -> bool {
        if x < 0 || x >= b.cols || y < 0 || y >= b.rows {
            return false;
        }
        let mut px = b.px;
        let mut row = px[y as usize];
        row[x as usize] = p.clone();
        px[y as usize] = row;
        b.px = px;
        return true;
    }
    fn SetPxRgb(b: Bitmap, x: i32, y: i32, c: i32) -> bool {
        return SetPx(&mut b.clone(), x, y, &pixelFromRgb(c));
    }
    fn GetPx(b: Bitmap, x: i32, y: i32) -> std::collections::HashMap<&'static str, i32> {
        if x < 0 || x >= b.cols || y < 0 || y >= b.rows {
            return { let mut m = std::collections::BTreeMap::new(); m.insert("ok", false); m };
        }
        let row = b.px[y as usize];
        return { let mut m = std::collections::BTreeMap::new(); m.insert("ok", true); m.insert("pixel", row[x as usize]); m };
    }
    fn GetPxRgb(b: Bitmap, x: i32, y: i32) -> std::collections::HashMap<&'static str, i32> {
        let r = GetPx(&b.clone(), x, y);
        if !r.ok != Default::default() {
            return { let mut m = std::collections::BTreeMap::new(); m.insert("ok", false); m };
        }
        return { let mut m = std::collections::BTreeMap::new(); m.insert("ok", true); m.insert("rgb", rgbFromPixel(&r.pixel)); m };
    }
    fn ppmSize(b: Bitmap) -> i32 {
        let header = format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "P6
# Creator: Rosetta Code http://rosettacode.org/
", b.cols.to_string()), " "), b.rows.to_string()), "
255
");
        return header.len() as i32 + 3 * b.cols * b.rows;
    }
    fn pixelStr(p: Pixel) -> &'static str {
        return format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "{", p.R.to_string()), " "), p.G.to_string()), " "), p.B.to_string()), "}");
    }
    fn main() -> () {
        let mut bm = NewBitmap(300, 240);
        FillRgb(&bm.clone(), 16711680);
        SetPxRgb(&bm.clone(), 10, 20, 255);
        SetPxRgb(&bm.clone(), 20, 30, 0);
        SetPxRgb(&bm.clone(), 30, 40, 1056816);
        let c1 = GetPx(&bm.clone(), 0, 0);
        let c2 = GetPx(&bm.clone(), 10, 20);
        let c3 = GetPx(&bm.clone(), 30, 40);
        println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", format!("{}{}", "Image size: ", bm.cols.to_string()), " × "), bm.rows.to_string()))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        println!("{}", vec![format!("{}", format!("{}{}", ppmSize(&bm.clone()).to_string(), " bytes when encoded as PPM."))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        if c1.ok != Default::default() {
            println!("{}", vec![format!("{}", format!("{}{}", "Pixel at (0,0) is ", pixelStr(&c1.pixel)))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        }
        if c2.ok != Default::default() {
            println!("{}", vec![format!("{}", format!("{}{}", "Pixel at (10,20) is ", pixelStr(&c2.pixel)))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        }
        if c3.ok != Default::default() {
            let p = c3.pixel;
            let mut r16 = (p.R as f64) * 257 as f64;
            let mut g16 = (p.G as f64) * 257 as f64;
            let mut b16 = (p.B as f64) * 257 as f64;
            println!("{}", vec![format!("{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "Pixel at (30,40) has R=", r16.to_string()), ", G="), g16.to_string()), ", B="), b16.to_string()))].into_iter().filter(|s| !s.is_empty()).collect::<Vec<_>>().join(" ") );
        }
    }
    main();
}
