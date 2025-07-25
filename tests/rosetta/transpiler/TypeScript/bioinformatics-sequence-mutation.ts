// Generated by Mochi v0.10.40 on 2025-07-25 17:41:35 GMT+7

function randInt(s: number, n: number): number[] {
  const next: number = (((s * 1664525) + 1013904223) % 2147483647);
  return [next, (next % n)];
}
function padLeft(s: string, w: number): string {
  let res: string = "";
  let n: number = (w - (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length));
  while ((n > 0)) {
    res = (res + " ");
    n = (n - 1);
  }
  return (res + s);
}
function makeSeq(s: number, le: number): any[] {
  const bases: string = "ACGT";
  let out: string = "";
  let i: number = 0;
  while ((i < le)) {
    let r: number[] = randInt(s, 4);
    s = r[Math.trunc(0)];
    const idx: number = Math.trunc(r[Math.trunc(1)]);
    out = (out + (bases).substring(idx, (idx + 1)));
    i = (i + 1);
  }
  return [s, out];
}
function mutate(s: number, dna: string, w: number[]): any[] {
  const bases: string = "ACGT";
  const le: number = (Array.isArray(dna) || typeof dna === 'string' ? dna.length : Object.keys(dna ?? {}).length);
  let r: number[] = randInt(s, le);
  s = r[Math.trunc(0)];
  const p: number = Math.trunc(r[Math.trunc(1)]);
  r = randInt(s, 300);
  s = r[Math.trunc(0)];
  const x: number = Math.trunc(r[Math.trunc(1)]);
  let arr: string[] = [];
  let i: number = 0;
  while ((i < le)) {
    arr = [...arr, (dna).substring(i, (i + 1))];
    i = (i + 1);
  }
  if ((x < w[Math.trunc(0)])) {
    r = randInt(s, 4);
    s = r[Math.trunc(0)];
    const idx: number = Math.trunc(r[Math.trunc(1)]);
    const b: string = (bases).substring(idx, (idx + 1));
    console.log((((((("  Change @" + padLeft(String(p), 3)) + " '") + arr[p]) + "' to '") + b) + "'"));
    arr[p] = b;
  } else {
    if ((x < (w[Math.trunc(0)] + w[Math.trunc(1)]))) {
      console.log((((("  Delete @" + padLeft(String(p), 3)) + " '") + arr[p]) + "'"));
      let j: number = p;
      while ((j < ((Array.isArray(arr) || typeof arr === 'string' ? arr.length : Object.keys(arr ?? {}).length) - 1))) {
        arr[j] = arr[Math.trunc((j + 1))];
        j = (j + 1);
      }
      arr = arr.slice(0, ((Array.isArray(arr) || typeof arr === 'string' ? arr.length : Object.keys(arr ?? {}).length) - 1));
    } else {
      r = randInt(s, 4);
      s = r[Math.trunc(0)];
      const idx2: number = Math.trunc(r[Math.trunc(1)]);
      const b: string = (bases).substring(idx2, (idx2 + 1));
      arr = [...arr, ""];
      let j: number = ((Array.isArray(arr) || typeof arr === 'string' ? arr.length : Object.keys(arr ?? {}).length) - 1);
      while ((j > p)) {
        arr[j] = arr[Math.trunc((j - 1))];
        j = (j - 1);
      }
      console.log((((("  Insert @" + padLeft(String(p), 3)) + " '") + b) + "'"));
      arr[p] = b;
    }
  }
  let out: string = "";
  i = 0;
  while ((i < (Array.isArray(arr) || typeof arr === 'string' ? arr.length : Object.keys(arr ?? {}).length))) {
    out = (out + arr[i]);
    i = (i + 1);
  }
  return [s, out];
}
function prettyPrint(dna: string, rowLen: number) {
  console.log("SEQUENCE:");
  const le: number = (Array.isArray(dna) || typeof dna === 'string' ? dna.length : Object.keys(dna ?? {}).length);
  let i: number = 0;
  while ((i < le)) {
    let k: number = (i + rowLen);
    if ((k > le)) {
      k = le;
    }
    console.log(((padLeft(String(i), 5) + ": ") + dna.slice(i, k)));
    i = (i + rowLen);
  }
  let a: number = 0;
  let c: number = 0;
  let g: number = 0;
  let t: number = 0;
  let idx: number = 0;
  while ((idx < le)) {
    const ch: string = (dna).substring(idx, (idx + 1));
    if ((ch == "A")) {
      a = (a + 1);
    } else {
      if ((ch == "C")) {
        c = (c + 1);
      } else {
        if ((ch == "G")) {
          g = (g + 1);
        } else {
          if ((ch == "T")) {
            t = (t + 1);
          }
        }
      }
    }
    idx = (idx + 1);
  }
  console.log("");
  console.log("BASE COUNT:");
  console.log(("    A: " + padLeft(String(a), 3)));
  console.log(("    C: " + padLeft(String(c), 3)));
  console.log(("    G: " + padLeft(String(g), 3)));
  console.log(("    T: " + padLeft(String(t), 3)));
  console.log("    ------");
  console.log(("    Σ: " + String(le)));
  console.log("    ======");
}
function wstring(w: number[]): string {
  return (((((("  Change: " + String(w[Math.trunc(0)])) + "\n  Delete: ") + String(w[Math.trunc(1)])) + "\n  Insert: ") + String(w[Math.trunc(2)])) + "\n");
}
function main() {
  let seed: number = 1;
  let res: any[] = makeSeq(seed, 250);
  seed = res[Math.trunc(0)];
  let dna: string = res[Math.trunc(1)];
  prettyPrint(dna, 50);
  const muts: number = 10;
  const w: number[] = [100, 100, 100];
  console.log("\nWEIGHTS (ex 300):");
  console.log(wstring(w));
  console.log((("MUTATIONS (" + String(muts)) + "):"));
  let i: number = 0;
  while ((i < muts)) {
    res = mutate(seed, dna, w);
    seed = res[Math.trunc(0)];
    dna = res[Math.trunc(1)];
    i = (i + 1);
  }
  console.log("");
  prettyPrint(dna, 50);
}
main();
