using System;
using System.Collections.Generic;
using System.Linq;

class Person { public string name; public int age; public string status; }

class Program {
    static void Main() {
        var people = new List<Person> {
            new Person{ name="Alice", age=17, status="minor" },
            new Person{ name="Bob", age=25, status="unknown" },
            new Person{ name="Charlie", age=18, status="unknown" },
            new Person{ name="Diana", age=16, status="minor" }
        };

        foreach (var p in people.Where(p => p.age >= 18)) {
            p.status = "adult";
            p.age = p.age + 1;
        }

        var expected = new List<Person> {
            new Person{ name="Alice", age=17, status="minor" },
            new Person{ name="Bob", age=26, status="adult" },
            new Person{ name="Charlie", age=19, status="adult" },
            new Person{ name="Diana", age=16, status="minor" }
        };

        bool ok = people.Zip(expected, (a,b) => a.name==b.name && a.age==b.age && a.status==b.status).All(x => x);
        if(!ok) throw new Exception("update failed");
        Console.WriteLine("ok");
    }
}
