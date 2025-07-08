const _tmp1 = [1, 2, 3];
for (const n of (Array.isArray(_tmp1) ? _tmp1 : Object.keys(_tmp1))) {
  console.log(n);
}
