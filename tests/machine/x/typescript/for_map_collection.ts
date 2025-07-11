let m = {a: 1, b: 2};
const _tmp1 = m;
for (const k of (Array.isArray(_tmp1) || typeof _tmp1 === "string" ? _tmp1 : Object.keys(_tmp1))) {
  console.log(k);
}
