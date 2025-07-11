interface Person {
  name: string;
  age: number;
  email: string;
}
const people = JSON.parse("[{\"age\":30,\"email\":\"alice@example.com\",\"name\":\"Alice\"},{\"age\":15,\"email\":\"bob@example.com\",\"name\":\"Bob\"},{\"age\":20,\"email\":\"charlie@example.com\",\"name\":\"Charlie\"}]");
const adults = people.filter((p) => (p.age >= 18)).map((p) => ({name: p.name, email: p.email}));
for (const a of adults) {
  console.log(a.name, a.email);
}
