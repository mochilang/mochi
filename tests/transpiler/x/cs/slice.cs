// Mochi 0.10.31 - generated 2025-07-19 10:59:06 UTC
using System;
using System.Linq;

class Program {
    static void Main() {
        Console.WriteLine(new[]{1, 2, 3}.Skip(1).Take((3 - 1)).ToArray());
        Console.WriteLine(new[]{1, 2, 3}.Skip(0).Take((2 - 0)).ToArray());
        Console.WriteLine("hello".Substring(1, (4 - 1)));
    }
}
