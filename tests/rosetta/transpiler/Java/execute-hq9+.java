public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static String bottles(int n) {
        if (n == 0) {
            return "No more bottles";
        }
        if (n == 1) {
            return "1 bottle";
        }
        return (String)(_p(n)) + " bottles";
    }

    static void sing99() {
        int i = 99;
        while (i > 0) {
            System.out.println(String.valueOf(bottles(i)) + " of beer on the wall");
            System.out.println(String.valueOf(bottles(i)) + " of beer");
            System.out.println("Take one down, pass it around");
            System.out.println(String.valueOf(bottles(i - 1)) + " of beer on the wall");
            i = i - 1;
        }
    }

    static void run(String code) {
        int acc = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(code)) {
            String ch = _substr(code, i_1, i_1 + 1);
            if ((ch.equals("H"))) {
                System.out.println("Hello, World!");
            } else             if ((ch.equals("Q"))) {
                System.out.println(code);
            } else             if ((ch.equals("9"))) {
                sing99();
            } else             if ((ch.equals("+"))) {
                acc = acc + 1;
            }
            i_1 = i_1 + 1;
        }
    }

    static void main() {
        String code = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        run(code);
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
