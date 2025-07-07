using System;

public class Todo {
    public string title { get; set; }
}

class Program {
    static void Main() {
        var todo = new Todo { title = "hi" };
        Console.WriteLine(todo.title);
    }
}
