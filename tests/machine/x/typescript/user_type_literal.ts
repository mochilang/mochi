type Person = { name: any; age: any; };
type Book = { title: any; author: any; };
let book = {title: "Go", author: {name: "Bob", age: 42}};
console.log(book.author.name);
