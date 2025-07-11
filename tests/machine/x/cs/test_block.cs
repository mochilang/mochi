using System;

class Program {
    static void test_addition_works() {
        int x = (1 + 2);
        expect((x == 3));
    }
    
    static void Main() {
        Console.WriteLine("ok");
        test_addition_works();
    }
    static void expect(bool cond) {
        if (!cond) throw new Exception("expect failed");
    }
    
}
