// Source: /workspace/mochi/tests/rosetta/x/Mochi/9-billion-names-of-god-the-integer.mochi

function bigTrim(a: number[]): number[] {
  var n = _count(a);
  while (((n > 1) && (a[n - 1] == 0))) {
    a = a.slice(0, n - 1);
    n = n - 1;
  }
  return a;
}

function bigFromInt(x: number): number[] {
  if ((x == 0)) {
    return [0];
  }
  var digits: number[] = [];
  var n = x;
  while ((n > 0)) {
    digits = [...digits, n % 10];
    n = Math.trunc(n / 10);
  }
  return digits;
}

function bigAdd(a: number[], b: number[]): number[] {
  var res: number[] = [];
  var carry = 0;
  var i = 0;
  while ((((i < _count(a)) || (i < _count(b))) || (carry > 0))) {
    var av = 0;
    if ((i < _count(a))) {
      av = a[i];
    }
    var bv = 0;
    if ((i < _count(b))) {
      bv = b[i];
    }
    var s = (av + bv) + carry;
    res = [...res, s % 10];
    carry = Math.trunc(s / 10);
    i = i + 1;
  }
  return bigTrim(res);
}

function bigSub(a: number[], b: number[]): number[] {
  var res: number[] = [];
  var borrow = 0;
  var i = 0;
  while ((i < _count(a))) {
    var av = a[i];
    var bv = 0;
    if ((i < _count(b))) {
      bv = b[i];
    }
    var diff = (av - bv) - borrow;
    if ((diff < 0)) {
      diff = diff + 10;
      borrow = 1;
    } else {
      borrow = 0;
    }
    res = [...res, diff];
    i = i + 1;
  }
  return bigTrim(res);
}

function bigToString(a: number[]): string {
  var s = "";
  var i = _count(a) - 1;
  while ((i >= 0)) {
    s = `${s}${String(a[i])}`;
    i = i - 1;
  }
  return s;
}

function minInt(a: number, b: number): number {
  if ((a < b)) {
    return a;
  } else {
    return b;
  }
}

function cumu(n: number): number[][] {
  var cache: number[][][] = [[bigFromInt(1)]];
  var y = 1;
  while ((y <= n)) {
    var row: number[][] = [bigFromInt(0)];
    var x = 1;
    while ((x <= y)) {
      let val = cache[y - x][minInt(x, y - x)];
      row = [...row, bigAdd(row[_count(row) - 1], val)];
      x = x + 1;
    }
    cache = [...cache, row];
    y = y + 1;
  }
  return cache[n];
}

function row(n: number): string[] {
  let e = cumu(n);
  var out: string[] = [];
  var i = 0;
  while ((i < n)) {
    let diff = bigSub(e[i + 1], e[i]);
    out = [...out, bigToString(diff)];
    i = i + 1;
  }
  return out;
}

var x: number;

function main(): void {
  console.log("rows:");
  x = 1;
  while ((x < 11)) {
    let r = row(x);
    var line = "";
    var i = 0;
    while ((i < _count(r))) {
      line = `${line} ${r[i]} `;
      i = i + 1;
    }
    console.log(line);
    x = x + 1;
  }
  console.log("");
  console.log("sums:");
  for (
    const num of [
      23,
      123,
      1234,
    ]
  ) {
    let r = cumu(num);
    console.log(`${String(num)} ${bigToString(r[_count(r) - 1])}`);
  }
}
function _count(v: any): number {
  if (Array.isArray(v)) return v.length;
  if (v && typeof v === "object") {
    if (Array.isArray((v as any).items)) return (v as any).items.length;
    if (Array.isArray((v as any).Items)) return (v as any).Items.length;
  }
  return 0;
}

main();
