// Generated by Mochi compiler v0.10.27 on 1970-01-01T00:00:00Z
// Source: /workspace/mochi/tests/vm/valid/update_stmt.mochi

type Person = {
  name: string;
  age: number;
  status: string;
};

let people: Person[];

function test_update_adult_status(): void {
  if (
    !(_equal(people, [
      {
        name: "Alice",
        age: 17,
        status: "minor",
      },
      {
        name: "Bob",
        age: 26,
        status: "adult",
      },
      {
        name: "Charlie",
        age: 19,
        status: "adult",
      },
      {
        name: "Diana",
        age: 16,
        status: "minor",
      },
    ]))
  ) throw new Error("expect failed");
}

function main(): void {
  people = [
    {
      name: "Alice",
      age: 17,
      status: "minor",
    },
    {
      name: "Bob",
      age: 25,
      status: "unknown",
    },
    {
      name: "Charlie",
      age: 18,
      status: "unknown",
    },
    {
      name: "Diana",
      age: 16,
      status: "minor",
    },
  ];
  for (let _i = 0; _i < people.length; _i++) {
    let _item = people[_i];
    let name = _item.name;
    let age = _item.age;
    let status = _item.status;
    if ((age >= 18)) {
      _item.status = "adult";
      _item.age = age + 1;
    }
    people[_i] = _item;
  }
  console.log("ok");
  test_update_adult_status();
}
function _equal(a: unknown, b: unknown): boolean {
  if (typeof a === "number" && typeof b === "number") {
    return Math.abs(a - b) < 1e-9;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (!_equal(a[i], b[i])) return false;
    return true;
  }
  if (a && b && typeof a === "object" && typeof b === "object") {
    const ak = Object.keys(a);
    const bk = Object.keys(b);
    if (ak.length !== bk.length) return false;
    for (const k of ak) {
      if (!bk.includes(k) || !_equal((a as any)[k], (b as any)[k])) {
        return false;
      }
    }
    return true;
  }
  return a === b;
}

main();
