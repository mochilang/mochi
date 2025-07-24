public class Main {
    static String err = "";

    static bigint pow_big(bigint base, int exp) {
        bigint result = 1;
        bigint b = base;
        int e = exp;
        while (e > 0) {
            if (e % 2 == 1) {
                result = result * b;
            }
            b = b * b;
            e = ((Number)((e / 2))).intValue();
        }
        return result;
    }

    static int bit_len(bigint x) {
        bigint n = x;
        int c = 0;
        while (n > 0) {
            n = n / 2;
            c = c + 1;
        }
        return c;
    }

    static bigint ackermann2(bigint m, bigint n) {
        if (!(err.equals(""))) {
            return 0;
        }
        if (m <= 3) {
            int mi = ((Number)(m)).intValue();
            if (mi == 0) {
                return n + 1;
            }
            if (mi == 1) {
                return n + 2;
            }
            if (mi == 2) {
                return 2 * n + 3;
            }
            if (mi == 3) {
                int nb = bit_len(n);
                if (nb > 64) {
                    err = "A(m,n) had n of " + String.valueOf(nb) + " bits; too large";
                    return 0;
                }
                bigint r = pow_big(2, ((Number)(n)).intValue());
                return 8 * r - 3;
            }
        }
        if (bit_len(n) == 0) {
            return ackermann2(m - (1), 1);
        }
        return ackermann2(m - (1), ackermann2(m, n - (1)));
    }

    static void show(int m, int n) {
        err = "";
        bigint res = ackermann2(m, n);
        if (!(err.equals(""))) {
            System.out.println("A(" + String.valueOf(m) + ", " + String.valueOf(n) + ") = Error: " + err);
            return;
        }
        if (bit_len(res) <= 256) {
            System.out.println("A(" + String.valueOf(m) + ", " + String.valueOf(n) + ") = " + String.valueOf(res));
        } else {
            String s = String.valueOf(res);
            String pre = s.substring(0, 20);
            String suf = s.substring(s.length() - 20, s.length());
            System.out.println("A(" + String.valueOf(m) + ", " + String.valueOf(n) + ") = " + String.valueOf(s.length()) + " digits starting/ending with: " + pre + "..." + suf);
        }
    }

    static void main() {
        show(0, 0);
        show(1, 2);
        show(2, 4);
        show(3, 100);
        show(3, 1000000);
        show(4, 1);
        show(4, 2);
        show(4, 3);
    }
    public static void main(String[] args) {
        main();
    }
}
