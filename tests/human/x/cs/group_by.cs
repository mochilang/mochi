using System;
using System.Collections.Generic;
using System.Linq;
class Person{public string name;public int age;public string city;}
class Program {
    static void Main(){
        var people=new List<Person>{
            new Person{name="Alice",age=30,city="Paris"},
            new Person{name="Bob",age=15,city="Hanoi"},
            new Person{name="Charlie",age=65,city="Paris"},
            new Person{name="Diana",age=45,city="Hanoi"},
            new Person{name="Eve",age=70,city="Paris"},
            new Person{name="Frank",age=22,city="Hanoi"}
        };
        var stats=people.GroupBy(p=>p.city)
            .Select(g=>new{city=g.Key,count=g.Count(),avg_age=g.Average(p=>p.age)});
        Console.WriteLine("--- People grouped by city ---");
        foreach(var s in stats){Console.WriteLine($"{s.city}: count = {s.count}, avg_age = {s.avg_age}");}
    }
}
