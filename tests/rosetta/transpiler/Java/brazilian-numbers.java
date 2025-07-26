public class Main {

    static boolean sameDigits(int n, int b) {
        int f = Math.floorMod(n, b);
        n = ((Number)((n / b))).intValue();
        while (n > 0) {
            if (Math.floorMod(n, b) != f) {
                return false;
            }
            n = ((Number)((n / b))).intValue();
        }
        return true;
    }

    static boolean isBrazilian(int n) {
        if (n < 7) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0 && n >= 8) {
            return true;
        }
        int b = 2;
        while (b < n - 1) {
            if (sameDigits(n, b)) {
                return true;
            }
            b = b + 1;
        }
        return false;
    }

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static void main() {
        String[] kinds = new String[]{" ", " odd ", " prime "};
        for (String kind : kinds) {
            System.out.println("First 20" + kind + "Brazilian numbers:");
            int c = 0;
            int n = 7;
            while (true) {
                if (isBrazilian(n)) {
                    System.out.println(String.valueOf(n) + " ");
                    c = c + 1;
                    if (c == 20) {
                        System.out.println("\n");
                        break;
                    }
                }
                if ((kind.equals(" "))) {
                    n = n + 1;
                } else                 if ((kind.equals(" odd "))) {
                    n = n + 2;
                } else {
                    while (true) {
                        n = n + 2;
                        if (isPrime(n)) {
                            break;
                        }
                    }
                }
            }
        }
        int n = 7;
        int c = 0;
        while (c < 100000) {
            if (isBrazilian(n)) {
                c = c + 1;
            }
            n = n + 1;
        }
        System.out.println("The 100,000th Brazilian number: " + String.valueOf(n - 1));
    }
    public static void main(String[] args) {
        main();
    }
}
