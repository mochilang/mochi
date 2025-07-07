using System;
using System.Collections.Generic;
class Program {
    static void Main() {
        var m = new Dictionary<string,int>{{"a",1},{"b",2}};
        foreach (var key in m.Keys) {
            Console.WriteLine(key);
        }
    }
}
