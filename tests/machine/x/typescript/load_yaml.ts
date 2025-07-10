type Person = { name: any; age: any; email: any; };
const people = JSON.parse("[{\"age\":30,\"email\":\"alice@example.com\",\"name\":\"Alice\"},{\"age\":15,\"email\":\"bob@example.com\",\"name\":\"Bob\"},{\"age\":20,\"email\":\"charlie@example.com\",\"name\":\"Charlie\"}]");
const adults = (() => {
  const _tmp48 = [];
  for (const p of people) {
    if (!((p.age >= 18))) continue;
    _tmp48.push({name: p.name, email: p.email});
  }
  let res = _tmp48;
  return res;
})()
;
const _tmp49 = adults;
for (const a of (Array.isArray(_tmp49) ? _tmp49 : Object.keys(_tmp49))) {
  console.log(a.name, a.email);
}
