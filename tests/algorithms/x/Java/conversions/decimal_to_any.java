public class Main {

    static String decimal_to_any(int num, int base) {
        if (num < 0) {
            throw new RuntimeException(String.valueOf("parameter must be positive int"));
        }
        if (base < 2) {
            throw new RuntimeException(String.valueOf("base must be >= 2"));
        }
        if (base > 36) {
            throw new RuntimeException(String.valueOf("base must be <= 36"));
        }
        if (num == 0) {
            return "0";
        }
        String symbols = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int n = num;
        String result = "";
        while (n > 0) {
            int mod = Math.floorMod(n, base);
            String digit = _substr(symbols, mod, mod + 1);
            result = digit + result;
            n = n / base;
        }
        return result;
    }

    static void main() {
        System.out.println(decimal_to_any(0, 2));
        System.out.println(decimal_to_any(5, 4));
        System.out.println(decimal_to_any(20, 3));
        System.out.println(decimal_to_any(58, 16));
        System.out.println(decimal_to_any(243, 17));
    }
    public static void main(String[] args) {
        main();
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
