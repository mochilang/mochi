using System;
using System.Collections.Generic;
using System.Linq;

class Program {
    static void Main() {
        List<People> people = new List<People> { new People { name = "Alice", age = 30 }, new People { name = "Bob", age = 15 }, new People { name = "Charlie", age = 65 }, new People { name = "Diana", age = 45 } };
        List<Adult> adults = people.Where(person => (person.age >= 18)).Select(person => new Adult { name = person.name, age = person.age, is_senior = (person.age >= 60) }).ToArray();
        Console.WriteLine("--- Adults ---");
        foreach (var person in adults) {
            Console.WriteLine(string.Join(" ", new [] { Convert.ToString(person.name), Convert.ToString("is"), Convert.ToString(person.age), Convert.ToString((person.is_senior ? " (senior)" : "")) }));
        }
    }
    public class People {
        public string name;
        public int age;
    }
    
    
    public class Adult {
        public string name;
        public int age;
        public bool is_senior;
    }
    
    
    
}
