use std::collections::{BTreeSet, HashMap, HashSet};
use std::io::{self, Read};

#[derive(Clone)]
struct Row {
    dept: String,
    name: String,
    salary: i32,
}

fn main() {
    let mut s = String::new();
    io::stdin().read_to_string(&mut s).unwrap();
    let mut it = s.split_whitespace();
    let t: usize = match it.next() { Some(v) => v.parse().unwrap(), None => return };
    let mut cases = Vec::new();
    for _ in 0..t {
        let d: usize = it.next().unwrap().parse().unwrap();
        let e: usize = it.next().unwrap().parse().unwrap();
        let mut dept_name = HashMap::new();
        for _ in 0..d {
            let id: i32 = it.next().unwrap().parse().unwrap();
            let name = it.next().unwrap().to_string();
            dept_name.insert(id, name);
        }
        let mut groups: HashMap<i32, Vec<(String, i32)>> = HashMap::new();
        for _ in 0..e {
            let _id: i32 = it.next().unwrap().parse().unwrap();
            let name = it.next().unwrap().to_string();
            let salary: i32 = it.next().unwrap().parse().unwrap();
            let dept_id: i32 = it.next().unwrap().parse().unwrap();
            groups.entry(dept_id).or_default().push((name, salary));
        }
        let mut out = Vec::new();
        for (dept_id, items) in groups {
            let mut uniq = BTreeSet::new();
            for (_, salary) in &items {
                uniq.insert(-salary);
            }
            let keep: HashSet<i32> = uniq.into_iter().take(3).map(|x| -x).collect();
            for (name, salary) in items {
                if keep.contains(&salary) {
                    out.push(Row { dept: dept_name[&dept_id].clone(), name, salary });
                }
            }
        }
        out.sort_by(|a, b| a.dept.cmp(&b.dept).then(b.salary.cmp(&a.salary)).then(a.name.cmp(&b.name)));
        let mut lines = vec![out.len().to_string()];
        for row in out {
            lines.push(format!("{},{},{}", row.dept, row.name, row.salary));
        }
        cases.push(lines.join("\n"));
    }
    print!("{}", cases.join("\n\n"));
}
