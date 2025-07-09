type Person = { name: any; age: any; email: any; };
let people = JSON.parse("[{\"age\":30,\"email\":\"alice@example.com\",\"name\":\"Alice\"},{\"age\":15,\"email\":\"bob@example.com\",\"name\":\"Bob\"},{\"age\":20,\"email\":\"charlie@example.com\",\"name\":\"Charlie\"}]");
let adults = (() => {
  const _tmp1 = [];
  for (const p of people) {
    if (!((p.age >= 18))) continue;
    _tmp1.push({name: p.name, email: p.email});
  }
  let res = _tmp1;
  return res;
})()
;
const _tmp2 = adults;
for (const a of (Array.isArray(_tmp2) ? _tmp2 : Object.keys(_tmp2))) {
  console.log(a.name, a.email);
}
