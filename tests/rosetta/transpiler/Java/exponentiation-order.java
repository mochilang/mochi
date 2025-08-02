public class Main {
    static int a;
    static int b;
    static int c;

    static int powInt(int b, int p) {
        int r = 1;
        int i = 0;
        while (i < p) {
            r = r * b;
            i = i + 1;
        }
        return r;
    }
    public static void main(String[] args) {
        a = powInt(5, powInt(3, 2));
        b = powInt(powInt(5, 3), 2);
        c = powInt(5, powInt(3, 2));
        System.out.println("5^3^2   = " + (String)(_p(a)));
        System.out.println("(5^3)^2 = " + (String)(_p(b)));
        System.out.println("5^(3^2) = " + (String)(_p(c)));
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
