type Person = { name: string; age: number; };
type Book = { title: string; author: Person; };
const book = {title: "Go", author: {name: "Bob", age: 42}};
console.log(book.author.name);
