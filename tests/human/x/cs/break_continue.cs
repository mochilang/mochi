using System;

class Program {
    static void Main() {
        int[] numbers = {1,2,3,4,5,6,7,8,9};
        foreach (var n in numbers) {
            if (n % 2 == 0) {
                continue;
            }
            if (n > 7) {
                break;
            }
            Console.WriteLine($"odd number: {n}");
        }
    }
}
