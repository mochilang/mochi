using System;
using System.Collections.Generic;
using System.Linq;

class Person { public string name; public int age; }

class Program {
    static void Main() {
        var people = new List<Person> {
            new Person{ name="Alice", age=30 },
            new Person{ name="Bob", age=15 },
            new Person{ name="Charlie", age=65 },
            new Person{ name="Diana", age=45 }
        };
        var adults = from person in people
                     where person.age >= 18
                     select new {
                         name = person.name,
                         age = person.age,
                         is_senior = person.age >= 60
                     };
        Console.WriteLine("--- Adults ---");
        foreach (var person in adults) {
            Console.WriteLine($"{person.name} is {person.age}{(person.is_senior ? " (senior)" : "")}");
        }
    }
}
