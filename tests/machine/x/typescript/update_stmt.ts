function deepEqual(a: any, b: any): boolean {
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!deepEqual(a[i], b[i])) return false;
    return true;
  }
  if (a && b && typeof a === 'object' && typeof b === 'object') {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) {
      if (!bk.includes(k) || !deepEqual(a[k], b[k])) return false;
    }
    return true;
  }
  return a === b;
}
type Person = { name: any; age: any; status: any; };
let people = [{name: "Alice", age: 17, status: "minor"}, {name: "Bob", age: 25, status: "unknown"}, {name: "Charlie", age: 18, status: "unknown"}, {name: "Diana", age: 16, status: "minor"}];
for (let i = 0; i < people.length; i++) {
  let _tmp1 = people[i];
  let status = _tmp1.status;
  let age = _tmp1.age;
  if ((age >= 18)) {
    _tmp1.status = "adult";
    _tmp1.age = (age + 1);
  }
  people[i] = _tmp1;
}
if (!(deepEqual(people, [{name: "Alice", age: 17, status: "minor"}, {name: "Bob", age: 26, status: "adult"}, {name: "Charlie", age: 19, status: "adult"}, {name: "Diana", age: 16, status: "minor"}]))) { throw new Error("update adult status failed"); }
console.log("ok");
