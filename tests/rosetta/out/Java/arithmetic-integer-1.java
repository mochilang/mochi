// arithmetic-integer-1.mochi
public class ArithmeticInteger1 {
    static void main() {
        int a = 12;
        int b = 8;
        System.out.println(String.valueOf(a) + " + " + String.valueOf(b) + " = " + String.valueOf(a + b));
        System.out.println(String.valueOf(a) + " - " + String.valueOf(b) + " = " + String.valueOf(a - b));
        System.out.println(String.valueOf(a) + " * " + String.valueOf(b) + " = " + String.valueOf(a * b));
        System.out.println(String.valueOf(a) + " / " + String.valueOf(b) + " = " + String.valueOf(Integer.parseInt((a / b))));
        System.out.println(String.valueOf(a) + " % " + String.valueOf(b) + " = " + String.valueOf(a % b));
    }
    public static void main(String[] args) {
    main();
    }
}
