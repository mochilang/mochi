using System;
using System.Collections.Generic;

public record struct Person {
    public string name;
    public int age;
    public string status;
}

class Program {
    static void test_update_adult_status() {
        expect((people == new List<Person> { new Person { name = "Alice", age = 17, status = "minor" }, new Person { name = "Bob", age = 26, status = "adult" }, new Person { name = "Charlie", age = 19, status = "adult" }, new Person { name = "Diana", age = 16, status = "minor" } }));
    }
    
    static void Main() {
        List<Person> people = new List<Person> { new Person { name = "Alice", age = 17, status = "minor" }, new Person { name = "Bob", age = 25, status = "unknown" }, new Person { name = "Charlie", age = 18, status = "unknown" }, new Person { name = "Diana", age = 16, status = "minor" } };
        for (int _tmp0 = 0; _tmp0 < people.Length; _tmp0++) {
            var _tmp1 = people[_tmp0];
            var name = _tmp1.name;
            var age = _tmp1.age;
            var status = _tmp1.status;
            if ((age >= 18)) {
                _tmp1.status = "adult";
                _tmp1.age = (age + 1);
            }
            people[_tmp0] = _tmp1;
        }
        Console.WriteLine("ok");
        test_update_adult_status();
    }
    static void expect(bool cond) {
        if (!cond) throw new Exception("expect failed");
    }
    
}
