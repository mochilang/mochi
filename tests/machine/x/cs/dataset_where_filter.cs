using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        var people = new List<dynamic> { new Dictionary<dynamic, dynamic> { { "name", "Alice" }, { "age", 30 } }, new Dictionary<dynamic, dynamic> { { "name", "Bob" }, { "age", 15 } }, new Dictionary<dynamic, dynamic> { { "name", "Charlie" }, { "age", 65 } }, new Dictionary<dynamic, dynamic> { { "name", "Diana" }, { "age", 45 } } };
        var adults = people.Where(person => (person.age >= 18)).Select(person => new Dictionary<dynamic, dynamic> { { "name", person.name }, { "age", person.age }, { "is_senior", (person.age >= 60) } }).ToArray();
        Console.WriteLine("--- Adults ---");
        foreach (var person in adults) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(person.name), Convert.ToString("is"), Convert.ToString(person.age), Convert.ToString((person.is_senior ? " (senior)" : "")) }));
        }
    }
}
