// Generated by Mochi v0.10.40 on 2025-07-25 17:41:38 GMT+7

import { createHash } from 'node:crypto';
function sha256(bs: number[]): number[] {
  const hash = createHash('sha256');
  hash.update(new Uint8Array(bs));
  return Array.from(hash.digest());
}
function indexOf(s: string, ch: string): number {
  let i: number = 0;
  while ((i < (Array.isArray(s) || typeof s === 'string' ? s.length : Object.keys(s ?? {}).length))) {
    if ((s.slice(i, (i + 1)) == ch)) {
      return i;
    }
    i = (i + 1);
  }
  return -1;
}
function set58(addr: string): number[] {
  const tmpl: string = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
  let a: number[] = [];
  let i: number = 0;
  while ((i < 25)) {
    a = [...a, 0];
    i = (i + 1);
  }
  let idx: number = 0;
  while ((idx < (Array.isArray(addr) || typeof addr === 'string' ? addr.length : Object.keys(addr ?? {}).length))) {
    const ch: string = addr.slice(idx, (idx + 1));
    let c: number = tmpl.indexOf(ch);
    if ((c < 0)) {
      return [];
    }
    let j: number = 24;
    while ((j >= 0)) {
      c = (c + (58 * a[j]));
      a[j] = (c % 256);
      c = Math.trunc(Math.trunc(c / 256));
      j = (j - 1);
    }
    if ((c > 0)) {
      return [];
    }
    idx = (idx + 1);
  }
  return a;
}
function doubleSHA256(bs: number[]): number[] {
  const first = sha256(bs);
  return sha256(first);
}
function computeChecksum(a: number[]): number[] {
  const hash: number[] = doubleSHA256(a.slice(0, 21));
  return hash.slice(0, 4);
}
function validA58(addr: string): boolean {
  const a: number[] = set58(addr);
  if (((Array.isArray(a) || typeof a === 'string' ? a.length : Object.keys(a ?? {}).length) != 25)) {
    return false;
  }
  if ((a[Math.trunc(0)] != 0)) {
    return false;
  }
  const sum = computeChecksum(a);
  let i: number = 0;
  while ((i < 4)) {
    if ((a[Math.trunc((21 + i))] != sum[i])) {
      return false;
    }
    i = (i + 1);
  }
  return true;
}
console.log(String(validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i")));
console.log(String(validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j")));
