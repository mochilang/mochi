// Generated by Mochi 0.10.34 on 2025-07-22 09:58 +0700
using System;
using System.Collections.Generic;
using System.Linq;

struct M {
    public int a;
    public int b;
    public int c;
    public override string ToString() => $"M {{a = {a}, b = {b}, c = {c}}}";
}
class Program {
    static M m = new M{a = 1, b = 2, c = 3};
    static void Main() {
        Console.WriteLine(("[" + (string.Join(", ", m.Values.ToList()) + "]")));
    }
}
