using System;

class Person
{
    public string name;
    public int age;
}

class Book
{
    public string title;
    public Person author;
}

class Program
{
    static void Main()
    {
        var book = new Book
        {
            title = "Go",
            author = new Person { name = "Bob", age = 42 },
        };
        Console.WriteLine(book.author.name);
    }
}
