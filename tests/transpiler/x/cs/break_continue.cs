// Mochi 0.10.31 - generated 2025-07-19 10:19:08 UTC
using System;

class Program {
    static void Main() {
        var numbers = new[]{1, 2, 3, 4, 5, 6, 7, 8, 9};
        foreach (var n in numbers) {
    if (((n % 2) == 0 ? 1 : 0) != 0) {
    continue;
};
    if ((n > 7 ? 1 : 0) != 0) {
    break;
};
    Console.WriteLine("odd number:", n);
};
    }
}
