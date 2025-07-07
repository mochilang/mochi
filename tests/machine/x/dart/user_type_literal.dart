class Person {
  String name;
  int age;
  Person({required this.name, required this.age});
}

class Book {
  String title;
  Person author;
  Book({required this.title, required this.author});
}

void main() {
  var book = Book(title: 'Go', author: Person(name: 'Bob', age: 42));
  print(book.author.name);
}
