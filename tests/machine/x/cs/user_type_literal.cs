using System;

public struct Person {
    public string name;
    public long age;
}

public struct Book {
    public string title;
    public Person author;
}

class Program {
    static void Main() {
        Book book = new Book { title = "Go", author = new Person { name = "Bob", age = 42 } };
        Console.WriteLine(book.author.name);
    }
}
