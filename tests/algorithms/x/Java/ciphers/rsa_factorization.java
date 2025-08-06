public class Main {

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static int pow_mod(int base, int exp, int mod) {
        int result = 1;
        int b = Math.floorMod(base, mod);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), mod);
            }
            e = e / 2;
            b = Math.floorMod((b * b), mod);
        }
        return result;
    }

    static int[] rsa_factor(int d, int e, int n) {
        int k = d * e - 1;
        int p = 0;
        int q = 0;
        int g = 2;
        while (p == 0 && g < n) {
            int t_1 = k;
            while (Math.floorMod(t_1, 2) == 0) {
                t_1 = t_1 / 2;
                int x_1 = pow_mod(g, t_1, n);
                int y_1 = gcd(x_1 - 1, n);
                if (x_1 > 1 && y_1 > 1) {
                    p = y_1;
                    q = n / y_1;
                    break;
                }
            }
            g = g + 1;
        }
        if (p > q) {
            return new int[]{q, p};
        }
        return new int[]{p, q};
    }
    public static void main(String[] args) {
        System.out.println(rsa_factor(3, 16971, 25777));
        System.out.println(rsa_factor(7331, 11, 27233));
        System.out.println(rsa_factor(4021, 13, 17711));
    }
}
