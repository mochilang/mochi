class Person {
    String name;
    int age;
    Person(String name, int age) {
        this.name = name;
        this.age = age;
    }
}

class Book {
    String title;
    Person author;
    Book(String title, Person author) {
        this.title = title;
        this.author = author;
    }
}

public class UserTypeLiteral {
    public static void main(String[] args) {
        Book book = new Book("Go", new Person("Bob", 42));
        System.out.println(book.author.name);
    }
}
