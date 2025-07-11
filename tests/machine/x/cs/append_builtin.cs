using System;
using System.Collections.Generic;

class Program {
    static void Main() {
        List<int> a = new List<int> { 1, 2 };
        Console.WriteLine(JsonSerializer.Serialize(new List<int>(a){3}));
    }
}
