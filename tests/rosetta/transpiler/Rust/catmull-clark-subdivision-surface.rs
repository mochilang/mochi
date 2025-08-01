// Generated by Mochi transpiler v0.10.50 on 2025-07-31 01:01 +0700
use std::collections::HashMap;
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
struct Point {
    x: f64,
    y: f64,
    z: f64,
}
impl std::fmt::Display for Point {
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
struct Edge {
    pn1: i64,
    pn2: i64,
    fn1: i64,
    fn2: i64,
    cp: Point,
}
impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"pn1\": {}", self.pn1)?;
        write!(f, ", ")?;
        write!(f, "\"pn2\": {}", self.pn2)?;
        write!(f, ", ")?;
        write!(f, "\"fn1\": {}", self.fn1)?;
        write!(f, ", ")?;
        write!(f, "\"fn2\": {}", self.fn2)?;
        write!(f, ", ")?;
        write!(f, "\"cp\": {}", self.cp)?;
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, Default)]
struct PointEx {
    p: Point,
    n: i64,
}
impl std::fmt::Display for PointEx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"p\": {}", self.p)?;
        write!(f, ", ")?;
        write!(f, "\"n\": {}", self.n)?;
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, Default)]
struct Ret {
    f0: Vec<Point>,
    f1: Vec<Vec<i64>>,
}
impl std::fmt::Display for Ret {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        write!(f, "\"f0\": {:?}", self.f0)?;
        write!(f, ", ")?;
        write!(f, "\"f1\": {:?}", self.f1)?;
        write!(f, "}}")
    }
}

fn main() {
        let _start_mem: i64 = _mem();
    let _start: i64 = _now();
    fn indexOf(s: &str, ch: &str) -> i64 {
    let mut i: i64 = 0;
    while (i < (s.len() as i64)) {
        if ({ let tmp = &s; tmp.chars().skip(i as usize).take(((i + 1) - i) as usize).collect::<String>() } == ch) {
            return i
        }
        i = (i + 1);
    }
    return -1
};
    fn fmt4(mut x: f64) -> String {
    let mut y: f64 = (x * 10000.0);
    if (y >= (0 as f64)) {
        y = (y + 0.5);
    } else {
        y = (y - 0.5);
    }
    y = (((y as i64) as f64) / 10000.0);
    let mut s = y.to_string();
    let mut dot: i64 = indexOf(&s, &".");
    if (dot == (0 - 1)) {
        s = format!("{}{}", s, ".0000");
    } else {
        let mut decs = (((s.len() as i64) - dot) - 1);
        if (decs > 4) {
            s = { let tmp = &s; tmp.chars().skip(0 as usize).take(((dot + 5) - 0) as usize).collect::<String>() };
        } else {
            while (decs < 4) {
                s = format!("{}{}", s, "0");
                decs = (decs + 1);
            }
        }
    }
    if (x >= 0.0) {
        s = format!("{}{}", " ", s);
    }
    return s.clone()
};
    fn fmt2(mut n: i64) -> String {
    let mut s = n.to_string();
    if ((s.len() as i64) < 2) {
        return format!("{}{}", " ", s)
    }
    return s.to_string().clone()
};
    fn sumPoint(p1: &Point, p2: &Point) -> Point {
    return Point {x: (p1.x + p2.x), y: (p1.y + p2.y), z: (p1.z + p2.z)}
};
    fn mulPoint(p: &Point, mut m: f64) -> Point {
    return Point {x: (p.x * m), y: (p.y * m), z: (p.z * m)}
};
    fn divPoint(p: &Point, mut d: f64) -> Point {
    return mulPoint(p, (1.0 / d))
};
    fn centerPoint(p1: &Point, p2: &Point) -> Point {
    return divPoint(&sumPoint(p1, p2), 2.0)
};
    fn getFacePoints(mut points: Vec<Point>, mut faces: Vec<Vec<i64>>) -> Vec<Point> {
    let mut facePoints: Vec<Point> = vec![];
    let mut i: i64 = 0;
    while (i < (faces.len() as i64)) {
        let mut face: Vec<i64> = faces[i as usize].clone();
        let mut fp: Point = Point {x: 0.0, y: 0.0, z: 0.0};
        for idx in face.clone() {
            fp = sumPoint(&fp, &points[idx as usize].clone());
        }
        fp = divPoint(&fp, ((face.len() as i64) as f64));
        facePoints = { let mut _v = facePoints.clone(); _v.push(fp.clone()); _v };
        i = (i + 1);
    }
    return facePoints
};
    fn sortEdges(mut edges: Vec<Vec<i64>>) -> Vec<Vec<i64>> {
    let mut res: Vec<Vec<i64>> = vec![];
    let mut tmp: Vec<Vec<i64>> = edges;
    while ((tmp.len() as i64) > 0) {
        let mut min: Vec<i64> = tmp[0 as usize].clone();
        let mut idx: i64 = 0;
        let mut j: i64 = 1;
        while (j < (tmp.len() as i64)) {
            let mut e: Vec<i64> = tmp[j as usize].clone();
            if ((e[0 as usize] < min[0 as usize]) || ((e[0 as usize] == min[0 as usize]) && ((e[1 as usize] < min[1 as usize]) || ((e[1 as usize] == min[1 as usize]) && (e[2 as usize] < min[2 as usize]))))) {
                min = e.clone();
                idx = j;
            }
            j = (j + 1);
        }
        res = { let mut _v = res.clone(); _v.push(min.clone()); _v };
        let mut out: Vec<Vec<i64>> = vec![];
        let mut k: i64 = 0;
        while (k < (tmp.len() as i64)) {
            if (k != idx) {
                out = { let mut _v = out.clone(); _v.push(tmp[k as usize].clone()); _v };
            }
            k = (k + 1);
        }
        tmp = out.clone();
    }
    return res
};
    fn getEdgesFaces(mut points: Vec<Point>, mut faces: Vec<Vec<i64>>) -> Vec<Edge> {
    let mut edges: Vec<Vec<i64>> = vec![];
    let mut fnum: i64 = 0;
    while (fnum < (faces.len() as i64)) {
        let mut face: Vec<i64> = faces[fnum as usize].clone();
        let mut numP = (face.len() as i64);
        let mut pi: i64 = 0;
        while (pi < numP) {
            let mut pn1: i64 = face[pi as usize];
            let mut pn2: i64 = 0;
            if (pi < (numP - 1)) {
                pn2 = face[(pi + 1) as usize];
            } else {
                pn2 = face[0 as usize];
            }
            if (pn1 > pn2) {
                let mut tmpn: i64 = pn1;
                pn1 = pn2;
                pn2 = tmpn;
            }
            edges = { let mut _v = edges.clone(); _v.push(vec![pn1, pn2, fnum]); _v };
            pi = (pi + 1);
        }
        fnum = (fnum + 1);
    }
    edges = sortEdges(edges.clone());
    let mut merged: Vec<Vec<i64>> = vec![];
    let mut idx: i64 = 0;
    while (idx < (edges.len() as i64)) {
        let mut e1: Vec<i64> = edges[idx as usize].clone();
        if (idx < ((edges.len() as i64) - 1)) {
            let mut e2: Vec<i64> = edges[(idx + 1) as usize].clone();
            if ((e1[0 as usize] == e2[0 as usize]) && (e1[1 as usize] == e2[1 as usize])) {
                merged = { let mut _v = merged.clone(); _v.push(vec![e1[0 as usize], e1[1 as usize], e1[2 as usize], e2[2 as usize]]); _v };
                idx = (idx + 2);
                continue
            }
        }
        merged = { let mut _v = merged.clone(); _v.push(vec![e1[0 as usize], e1[1 as usize], e1[2 as usize], -1]); _v };
        idx = (idx + 1);
    }
    let mut edgesCenters: Vec<Edge> = vec![];
    for me in &merged {
        let mut p1: Point = points[me[0 as usize] as usize].clone();
        let mut p2: Point = points[me[1 as usize] as usize].clone();
        let mut cp: Point = centerPoint(&p1, &p2);
        edgesCenters = { let mut _v = edgesCenters.clone(); _v.push(Edge {pn1: me[0 as usize], pn2: me[1 as usize], fn1: me[2 as usize], fn2: me[3 as usize], cp: cp}); _v };
    }
    return edgesCenters
};
    fn getEdgePoints(mut points: Vec<Point>, mut edgesFaces: Vec<Edge>, mut facePoints: Vec<Point>) -> Vec<Point> {
    let mut edgePoints: Vec<Point> = vec![];
    let mut i: i64 = 0;
    while (i < (edgesFaces.len() as i64)) {
        let mut edge: Edge = edgesFaces[i as usize].clone();
        let mut cp: Point = edge.cp;
        let mut fp1: Point = facePoints[edge.fn1 as usize].clone();
        let mut fp2: Point = fp1.clone();
        if (edge.fn2 != (0 - 1)) {
            fp2 = facePoints[edge.fn2 as usize].clone();
        }
        let mut cfp: Point = centerPoint(&fp1, &fp2);
        edgePoints = { let mut _v = edgePoints.clone(); _v.push(centerPoint(&cp, &cfp)); _v };
        i = (i + 1);
    }
    return edgePoints
};
    fn getAvgFacePoints(mut points: Vec<Point>, mut faces: Vec<Vec<i64>>, mut facePoints: Vec<Point>) -> Vec<Point> {
    let mut numP = (points.len() as i64);
    let mut temp: Vec<PointEx> = vec![];
    let mut i: i64 = 0;
    while (i < numP) {
        temp = { let mut _v = temp.clone(); _v.push(PointEx {p: Point {x: 0.0, y: 0.0, z: 0.0}, n: 0}); _v };
        i = (i + 1);
    }
    let mut fnum: i64 = 0;
    while (fnum < (faces.len() as i64)) {
        let mut fp: Point = facePoints[fnum as usize].clone();
        for pn in faces[fnum as usize].clone() {
            let mut tp: PointEx = temp[pn as usize].clone();
            temp[pn as usize] = PointEx {p: sumPoint(&tp.p, &fp), n: (tp.n + 1)};
        }
        fnum = (fnum + 1);
    }
    let mut avg: Vec<Point> = vec![];
    let mut j: i64 = 0;
    while (j < numP) {
        let mut tp: PointEx = temp[j as usize].clone();
        avg = { let mut _v = avg.clone(); _v.push(divPoint(&tp.p, (tp.n as f64))); _v };
        j = (j + 1);
    }
    return avg
};
    fn getAvgMidEdges(mut points: Vec<Point>, mut edgesFaces: Vec<Edge>) -> Vec<Point> {
    let mut numP = (points.len() as i64);
    let mut temp: Vec<PointEx> = vec![];
    let mut i: i64 = 0;
    while (i < numP) {
        temp = { let mut _v = temp.clone(); _v.push(PointEx {p: Point {x: 0.0, y: 0.0, z: 0.0}, n: 0}); _v };
        i = (i + 1);
    }
    for edge in &edgesFaces {
        let mut cp: Point = edge.cp.clone();
        let mut arr: Vec<i64> = vec![edge.pn1, edge.pn2];
        for pn in arr.clone() {
            let mut tp: PointEx = temp[pn as usize].clone();
            temp[pn as usize] = PointEx {p: sumPoint(&tp.p, &cp), n: (tp.n + 1)};
        }
    }
    let mut avg: Vec<Point> = vec![];
    let mut j: i64 = 0;
    while (j < numP) {
        let mut tp: PointEx = temp[j as usize].clone();
        avg = { let mut _v = avg.clone(); _v.push(divPoint(&tp.p, (tp.n as f64))); _v };
        j = (j + 1);
    }
    return avg
};
    fn getPointsFaces(mut points: Vec<Point>, mut faces: Vec<Vec<i64>>) -> Vec<i64> {
    let mut pf: Vec<i64> = vec![];
    let mut i: i64 = 0;
    while (i < (points.len() as i64)) {
        pf = { let mut _v = pf.clone(); _v.push(0); _v };
        i = (i + 1);
    }
    let mut fnum: i64 = 0;
    while (fnum < (faces.len() as i64)) {
        for pn in faces[fnum as usize].clone() {
            pf[pn as usize] = (pf[pn as usize] + 1);
        }
        fnum = (fnum + 1);
    }
    return pf
};
    fn getNewPoints(mut points: Vec<Point>, mut pf: Vec<i64>, mut afp: Vec<Point>, mut ame: Vec<Point>) -> Vec<Point> {
    let mut newPts: Vec<Point> = vec![];
    let mut i: i64 = 0;
    while (i < (points.len() as i64)) {
        let mut n: f64 = (pf[i as usize] as f64);
        let mut m1: f64 = ((n - 3.0) / n);
        let mut m2: f64 = (1.0 / n);
        let mut m3: f64 = (2.0 / n);
        let mut old: Point = points[i as usize].clone();
        let mut p1: Point = mulPoint(&old, m1);
        let mut p2: Point = mulPoint(&afp[i as usize].clone(), m2);
        let mut p3: Point = mulPoint(&ame[i as usize].clone(), m3);
        newPts = { let mut _v = newPts.clone(); _v.push(sumPoint(&sumPoint(&p1, &p2), &p3)); _v };
        i = (i + 1);
    }
    return newPts
};
    fn key(mut a: i64, mut b: i64) -> String {
    if (a < b) {
        return format!("{}{}", format!("{}{}", a.to_string(), ","), b.to_string()).to_string()
    }
    return format!("{}{}", format!("{}{}", b.to_string(), ","), a.to_string()).to_string().clone()
};
    fn cmcSubdiv(mut points: Vec<Point>, mut faces: Vec<Vec<i64>>) -> Ret {
    let mut facePoints: Vec<Point> = getFacePoints(points.clone(), faces.clone());
    let mut edgesFaces: Vec<Edge> = getEdgesFaces(points.clone(), faces.clone());
    let mut edgePoints: Vec<Point> = getEdgePoints(points.clone(), edgesFaces.clone(), facePoints.clone());
    let mut avgFacePoints: Vec<Point> = getAvgFacePoints(points.clone(), faces.clone(), facePoints.clone());
    let mut avgMidEdges: Vec<Point> = getAvgMidEdges(points.clone(), edgesFaces.clone());
    let mut pointsFaces: Vec<i64> = getPointsFaces(points.clone(), faces.clone());
    let mut newPoints: Vec<Point> = getNewPoints(points.clone(), pointsFaces.clone(), avgFacePoints.clone(), avgMidEdges.clone());
    let mut facePointNums: Vec<i64> = vec![];
    let mut nextPoint = (newPoints.len() as i64);
    for fp in &facePoints {
        newPoints = { let mut _v = newPoints.clone(); _v.push(fp.clone()); _v };
        facePointNums = { let mut _v = facePointNums.clone(); _v.push(nextPoint); _v };
        nextPoint = (nextPoint + 1);
    }
    let mut edgePointNums: HashMap<String, i64> = HashMap::from([]);
    let mut idx: i64 = 0;
    while (idx < (edgesFaces.len() as i64)) {
        let mut e: Edge = edgesFaces[idx as usize].clone();
        newPoints = { let mut _v = newPoints.clone(); _v.push(edgePoints[idx as usize].clone()); _v };
        edgePointNums.insert(key(e.pn1, e.pn2).clone(), nextPoint);
        nextPoint = (nextPoint + 1);
        idx = (idx + 1);
    }
    let mut newFaces: Vec<Vec<i64>> = vec![];
    let mut fnum: i64 = 0;
    while (fnum < (faces.len() as i64)) {
        let mut oldFace: Vec<i64> = faces[fnum as usize].clone();
        if ((oldFace.len() as i64) == 4) {
            let mut a: i64 = oldFace[0 as usize];
            let mut b: i64 = oldFace[1 as usize];
            let mut c: i64 = oldFace[2 as usize];
            let mut d: i64 = oldFace[3 as usize];
            let mut fpnum: i64 = facePointNums[fnum as usize];
            let mut ab: i64 = edgePointNums[key(a, b).as_str()];
            let mut da: i64 = edgePointNums[key(d, a).as_str()];
            let mut bc: i64 = edgePointNums[key(b, c).as_str()];
            let mut cd: i64 = edgePointNums[key(c, d).as_str()];
            newFaces = { let mut _v = newFaces.clone(); _v.push(vec![a, ab, fpnum, da]); _v };
            newFaces = { let mut _v = newFaces.clone(); _v.push(vec![b, bc, fpnum, ab]); _v };
            newFaces = { let mut _v = newFaces.clone(); _v.push(vec![c, cd, fpnum, bc]); _v };
            newFaces = { let mut _v = newFaces.clone(); _v.push(vec![d, da, fpnum, cd]); _v };
        }
        fnum = (fnum + 1);
    }
    return Ret {f0: newPoints, f1: newFaces}
};
    fn formatPoint(p: &Point) -> String {
    return format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", format!("{}{}", "[", fmt4(p.x)), " "), fmt4(p.y)), " "), fmt4(p.z)), "]").clone()
};
    fn formatFace(mut f: Vec<i64>) -> String {
    if ((f.len() as i64) == 0) {
        return String::from("[]")
    }
    let mut s: String = format!("{}{}", "[", fmt2(f[0 as usize])).clone();
    let mut i: i64 = 1;
    while (i < (f.len() as i64)) {
        s = format!("{}{}", format!("{}{}", s, " "), fmt2(f[i as usize]));
        i = (i + 1);
    }
    s = format!("{}{}", s, "]");
    return s.clone()
};
    fn mochi_main() {
    let mut inputPoints: Vec<Point> = vec![Point {x: -1.0, y: 1.0, z: 1.0}, Point {x: -1.0, y: -1.0, z: 1.0}, Point {x: 1.0, y: -1.0, z: 1.0}, Point {x: 1.0, y: 1.0, z: 1.0}, Point {x: 1.0, y: -1.0, z: -1.0}, Point {x: 1.0, y: 1.0, z: -1.0}, Point {x: -1.0, y: -1.0, z: -1.0}, Point {x: -1.0, y: 1.0, z: -1.0}];
    let mut inputFaces: Vec<Vec<i64>> = vec![vec![0, 1, 2, 3], vec![3, 2, 4, 5], vec![5, 4, 6, 7], vec![7, 0, 3, 5], vec![7, 6, 1, 0], vec![6, 1, 2, 4]];
    let mut outputPoints: Vec<Point> = inputPoints;
    let mut outputFaces: Vec<Vec<i64>> = inputFaces;
    let mut i: i64 = 0;
    while (i < 1) {
        let mut res: Ret = cmcSubdiv(outputPoints.clone(), outputFaces.clone());
        outputPoints = res.f0.clone();
        outputFaces = res.f1.clone();
        i = (i + 1);
    }
    for p in outputPoints {
        println!("{}", formatPoint(&p));
    }
    println!("{}", "");
    for f in outputFaces {
        println!("{}", formatFace(f.clone()));
    }
};
    mochi_main();
    let _end: i64 = _now();
    let _end_mem: i64 = _mem();
    let duration_us: i64 = ((_end - _start) / 1000);
    let memory_bytes: i64 = (_end_mem - _start_mem);
    println!("{{\n  \"duration_us\": {},\n  \"memory_bytes\": {},\n  \"name\": \"{}\"\n}}", duration_us, memory_bytes, "main");

}
