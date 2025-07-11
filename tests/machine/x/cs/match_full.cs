using System;

class Program {
    static string classify(int n) {
        return new Func<string>(() => {
        var _t = n;
        if (_equal(_t, 0)) return "zero";
        if (_equal(_t, 1)) return "one";
        return "many";
    })();
    }
    
    static void Main() {
        int x = 2;
        string label = new Func<string>(() => {
        var _t = x;
        if (_equal(_t, 1)) return "one";
        if (_equal(_t, 2)) return "two";
        if (_equal(_t, 3)) return "three";
        return "unknown";
    })();
        Console.WriteLine(label);
        string day = "sun";
        string mood = new Func<string>(() => {
        var _t = day;
        if (_equal(_t, "mon")) return "tired";
        if (_equal(_t, "fri")) return "excited";
        if (_equal(_t, "sun")) return "relaxed";
        return "normal";
    })();
        Console.WriteLine(mood);
        bool ok = true;
        string status = new Func<string>(() => {
        var _t = ok;
        if (_equal(_t, true)) return "confirmed";
        if (_equal(_t, false)) return "denied";
        return default;
    })();
        Console.WriteLine(status);
        Console.WriteLine(classify(0));
        Console.WriteLine(classify(5));
    }
    static bool _equal(dynamic a, dynamic b) {
        if (a is System.Collections.IEnumerable ae && b is System.Collections.IEnumerable be && a is not string && b is not string) {
            var ea = ae.GetEnumerator();
            var eb = be.GetEnumerator();
            while (true) {
                bool ha = ea.MoveNext();
                bool hb = eb.MoveNext();
                if (ha != hb) return false;
                if (!ha) break;
                if (!_equal(ea.Current, eb.Current)) return false;
            }
            return true;
        }
        if ((a is int || a is long || a is float || a is double) && (b is int || b is long || b is float || b is double)) {
            return Convert.ToDouble(a) == Convert.ToDouble(b);
        }
        return Equals(a, b);
    }
    
}
