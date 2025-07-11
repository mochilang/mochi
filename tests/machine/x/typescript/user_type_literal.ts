interface Person { name: string; age: number; }
interface Book { title: string; author: Person; }
const book = {title: "Go", author: {name: "Bob", age: 42}};
console.log(book.author.name);
