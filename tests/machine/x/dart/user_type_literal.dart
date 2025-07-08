class Person {
  String name;
  int age;
  Person(this.name, this.age);
}

class Book {
  String title;
  Person author;
  Book(this.title, this.author);
}

void main() {
  var book = Book('Go', Person('Bob', 42));
  print(book.author.name);
}
