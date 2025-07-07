using System;

class Program {
    static string Classify(int n) => n switch { 0 => "zero", 1 => "one", _ => "many" };

    static void Main() {
        int x = 2;
        string label = x switch {
            1 => "one",
            2 => "two",
            3 => "three",
            _ => "unknown"
        };
        Console.WriteLine(label);

        string day = "sun";
        string mood = day switch {
            "mon" => "tired",
            "fri" => "excited",
            "sun" => "relaxed",
            _ => "normal"
        };
        Console.WriteLine(mood);

        bool ok = true;
        string status = ok switch {
            true => "confirmed",
            false => "denied"
        };
        Console.WriteLine(status);

        Console.WriteLine(Classify(0));
        Console.WriteLine(Classify(5));
    }
}
