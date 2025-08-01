// Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:12 +0700
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};
static NOW_SEEDED: AtomicBool = AtomicBool::new(false);
static NOW_SEED: AtomicI64 = AtomicI64::new(0);
fn _now() -> i64 {
    if !NOW_SEEDED.load(Ordering::SeqCst) {
        if let Ok(s) = std::env::var("MOCHI_NOW_SEED") {
            if let Ok(v) = s.parse::<i64>() {
                NOW_SEED.store(v, Ordering::SeqCst);
                NOW_SEEDED.store(true, Ordering::SeqCst);
            }
        }
    }
    if NOW_SEEDED.load(Ordering::SeqCst) {
        let seed = (NOW_SEED.load(Ordering::SeqCst)*1664525 + 1013904223) % 2147483647;
        NOW_SEED.store(seed, Ordering::SeqCst);
        seed
    } else {
        SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos() as i64
    }
}
fn _mem() -> i64 {
    if let Ok(mut f) = std::fs::File::open("/proc/self/statm") {
        let mut s = String::new();
        use std::io::Read;
        if f.read_to_string(&mut s).is_ok() {
            if let Some(p) = s.split_whitespace().next() {
                if let Ok(v) = p.parse::<i64>() {
                    return v * 4096;
                }
            }
        }
    }
    0
}
#[derive(Debug, Clone, Default)]
struct Point3 {
    x: f64,
    y: f64,
    z: f64,
}
impl std::fmt::Display for Point3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"x\": {}", self.x)?;
        write!(f, ", ")?;
        write!(f, "\"y\": {}", self.y)?;
        write!(f, ", ")?;
        write!(f, "\"z\": {}", self.z)?;
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, Default)]
struct Point2 {
    x: i64,
    y: i64,
}
impl std::fmt::Display for Point2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"x\": {}", self.x)?;
        write!(f, ", ")?;
        write!(f, "\"y\": {}", self.y)?;
        write!(f, "}}")
    }
}

static mut g_PI: f64 = 0.0;
static mut g_TWO_PI: f64 = 0.0;
static mut g_nodes: Vec<Point3> = Vec::new();
static mut g_edges: Vec<Vec<i64>> = Vec::new();
static mut g_width: i64 = 0;
static mut g_height: i64 = 0;
static mut g_distance: f64 = 0.0;
static mut g_scale: f64 = 0.0;
fn main() {
    unsafe {
        g_PI = 3.141592653589793;
        g_TWO_PI = 6.283185307179586;
        g_nodes = vec![Point3 {x: -1.0, y: -1.0, z: -1.0}, Point3 {x: -1.0, y: -1.0, z: 1.0}, Point3 {x: -1.0, y: 1.0, z: -1.0}, Point3 {x: -1.0, y: 1.0, z: 1.0}, Point3 {x: 1.0, y: -1.0, z: -1.0}, Point3 {x: 1.0, y: -1.0, z: 1.0}, Point3 {x: 1.0, y: 1.0, z: -1.0}, Point3 {x: 1.0, y: 1.0, z: 1.0}];
        g_edges = vec![vec![0, 1], vec![1, 3], vec![3, 2], vec![2, 0], vec![4, 5], vec![5, 7], vec![7, 6], vec![6, 4], vec![0, 4], vec![1, 5], vec![2, 6], vec![3, 7]];
        g_width = 40;
        g_height = 20;
        g_distance = 3.0;
        g_scale = 8.0;
                let _start_mem: i64 = _mem();
        let _start: i64 = _now();
        static mut g_PI: f64 = 0.0;;
        static mut g_TWO_PI: f64 = 0.0;;
        unsafe fn _mod(mut x: f64, mut m: f64) -> f64 {
    return (x - ((((x / m) as i64) as f64) * m))
};
        unsafe fn _sin(mut x: f64) -> f64 {
    let mut y: f64 = (_mod((x + g_PI), g_TWO_PI) - g_PI);
    let mut y2: f64 = (y * y);
    let mut y3: f64 = (y2 * y);
    let mut y5: f64 = (y3 * y2);
    let mut y7: f64 = (y5 * y2);
    return (((y - (y3 / 6.0)) + (y5 / 120.0)) - (y7 / 5040.0))
};
        unsafe fn _cos(mut x: f64) -> f64 {
    let mut y: f64 = (_mod((x + g_PI), g_TWO_PI) - g_PI);
    let mut y2: f64 = (y * y);
    let mut y4: f64 = (y2 * y2);
    let mut y6: f64 = (y4 * y2);
    return (((1.0 - (y2 / 2.0)) + (y4 / 24.0)) - (y6 / 720.0))
};
        static mut g_nodes: Vec<Point3> = Vec::new();;
        static mut g_edges: Vec<Vec<i64>> = Vec::new();;
        unsafe fn rotate(p: &Point3, mut ax: f64, mut ay: f64) -> Point3 {
    let mut sinx: f64 = _sin(ax);
    let mut cosx: f64 = _cos(ax);
    let mut siny: f64 = _sin(ay);
    let mut cosy: f64 = _cos(ay);
    let mut x1: f64 = p.x;
    let mut y1: f64 = ((p.y * cosx) - (p.z * sinx));
    let mut z1: f64 = ((p.y * sinx) + (p.z * cosx));
    let mut x2: f64 = ((x1 * cosy) + (z1 * siny));
    let mut z2: f64 = ((-x1 * siny) + (z1 * cosy));
    return Point3 {x: x2, y: y1, z: z2}
};
        static mut g_width: i64 = 0;;
        static mut g_height: i64 = 0;;
        static mut g_distance: f64 = 0.0;;
        static mut g_scale: f64 = 0.0;;
        unsafe fn project(p: &Point3) -> Point2 {
    let mut factor: f64 = (g_scale / (p.z + g_distance));
    let mut x: i64 = (((p.x * factor) as i64) + (g_width / 2));
    let mut y: i64 = (((-p.y * factor) as i64) + (g_height / 2));
    return Point2 {x: x, y: y}
};
        unsafe fn clearGrid() -> Vec<Vec<String>> {
    let mut g: Vec<Vec<String>> = vec![];
    let mut y: i64 = 0;
    while (y < g_height) {
        let mut row: Vec<String> = vec![];
        let mut x: i64 = 0;
        while (x < g_width) {
            row = { let mut _v = row.clone(); _v.push(" ".to_string()); _v };
            x = (x + 1);
        }
        g = { let mut _v = g.clone(); _v.push(row.clone()); _v };
        y = (y + 1);
    }
    return g
};
        unsafe fn drawPoint(g: &mut Vec<Vec<String>>, mut x: i64, mut y: i64, ch: &str) {
    if ((((x >= 0) && (x < g_width)) && (y >= 0)) && (y < g_height)) {
        let mut row: Vec<String> = g[y as usize].clone();
        row[x as usize] = ch;
        g[y as usize] = row;
    }
};
        unsafe fn bresenham(mut x0: i64, mut y0: i64, mut x1: i64, mut y1: i64, mut g: Vec<Vec<String>>, ch: &str) {
    let mut dx: i64 = (x1 - x0);
    if (dx < 0) {
        dx = -dx;
    }
    let mut dy: i64 = (y1 - y0);
    if (dy < 0) {
        dy = -dy;
    }
    let mut sx: i64 = -1;
    if (x0 < x1) {
        sx = 1;
    }
    let mut sy: i64 = -1;
    if (y0 < y1) {
        sy = 1;
    }
    let mut err: i64 = (dx - dy);
    loop {
        drawPoint(&mut g, x0, y0, ch);
        if ((x0 == x1) && (y0 == y1)) {
            break
        }
        let mut e2: i64 = (2 * err);
        if (e2 > -dy) {
            err = (err - dy);
            x0 = (x0 + sx);
        }
        if (e2 < dx) {
            err = (err + dx);
            y0 = (y0 + sy);
        }
    }
};
        unsafe fn render(mut g: Vec<Vec<String>>) -> String {
    let mut out = String::from("");
    let mut y: i64 = 0;
    while (y < g_height) {
        let mut line = String::from("");
        let mut x: i64 = 0;
        while (x < g_width) {
            line = format!("{}{}", line, g[y as usize].clone()[x as usize].clone());
            x = (x + 1);
        }
        out = format!("{}{}", format!("{}{}", out, line), "\n");
        y = (y + 1);
    }
    return out.clone()
};
        unsafe fn mochi_main() {
    let mut f: i64 = 0;
    while (f < 10) {
        let mut grid: Vec<Vec<String>> = clearGrid();
        let mut rot: Vec<Point2> = vec![];
        let mut i: i64 = 0;
        let mut ay: f64 = ((g_PI / 4.0) + (((f as f64) * g_PI) / 10.0));
        while (i < (g_nodes.clone().len() as i64)) {
            let mut p: Point3 = rotate(&g_nodes[i as usize].clone(), (g_PI / 4.0), ay);
            let mut pp: Point2 = project(&p);
            rot = { let mut _v = rot.clone(); _v.push(pp.clone()); _v };
            i = (i + 1);
        }
        let mut e: i64 = 0;
        while (e < (g_edges.clone().len() as i64)) {
            let mut a: i64 = g_edges.clone()[e as usize].clone()[0 as usize];
            let mut b: i64 = g_edges.clone()[e as usize].clone()[1 as usize];
            let mut p1: Point2 = rot[a as usize].clone();
            let mut p2: Point2 = rot[b as usize].clone();
            bresenham(p1.x, p1.y, p2.x, p2.y, grid.clone(), &"#");
            e = (e + 1);
        }
        println!("{}", render(grid.clone()));
        f = (f + 1);
    }
};
        mochi_main();
        let _end: i64 = _now();
        let _end_mem: i64 = _mem();
        let duration_us: i64 = ((_end - _start) / 1000);
        let memory_bytes: i64 = (_end_mem - _start_mem);
        println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

    }
}
