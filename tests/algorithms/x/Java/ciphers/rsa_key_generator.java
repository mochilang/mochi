public class Main {
    static int seed = 0;
    static class Keys {
        int[] public_key;
        int[] private_key;
        Keys(int[] public_key, int[] private_key) {
            this.public_key = public_key;
            this.private_key = private_key;
        }
        Keys() {}
        @Override public String toString() {
            return String.format("{'public_key': %s, 'private_key': %s}", String.valueOf(public_key), String.valueOf(private_key));
        }
    }

    static Keys keys;
    static int[] pub;
    static int[] priv;

    static int pow2(int exp) {
        int res = 1;
        int i = 0;
        while (i < exp) {
            res = res * 2;
            i = i + 1;
        }
        return res;
    }

    static int next_seed(int x) {
        return ((int)(Math.floorMod(((long)((x * 1103515245 + 12345))), 2147483648L)));
    }

    static int rand_range(int min, int max) {
        seed = next_seed(seed);
        return min + Math.floorMod(seed, (max - min));
    }

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int temp = Math.floorMod(x, y);
            x = y;
            y = temp;
        }
        return x;
    }

    static int mod_inverse(int e, int phi) {
        int t = 0;
        int newt = 1;
        int r = phi;
        int newr = e;
        while (newr != 0) {
            int quotient = r / newr;
            int tmp = newt;
            newt = t - quotient * newt;
            t = tmp;
            int tmp_r = newr;
            newr = r - quotient * newr;
            r = tmp_r;
        }
        if (r > 1) {
            return 0;
        }
        if (t < 0) {
            t = t + phi;
        }
        return t;
    }

    static boolean is_prime(int n) {
        if (n < 2) {
            return false;
        }
        int i_1 = 2;
        while (i_1 * i_1 <= n) {
            if (Math.floorMod(n, i_1) == 0) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static int generate_prime(int bits) {
        int min = pow2(bits - 1);
        int max = pow2(bits);
        int p = rand_range(min, max);
        if (Math.floorMod(p, 2) == 0) {
            p = p + 1;
        }
        while (!(Boolean)is_prime(p)) {
            p = p + 2;
            if (p >= max) {
                p = min + 1;
            }
        }
        return p;
    }

    static Keys generate_key(int bits) {
        int p_1 = generate_prime(bits);
        int q = generate_prime(bits);
        int n = p_1 * q;
        int phi = (p_1 - 1) * (q - 1);
        int e = rand_range(2, phi);
        while (gcd(e, phi) != 1) {
            e = e + 1;
            if (e >= phi) {
                e = 2;
            }
        }
        int d = mod_inverse(e, phi);
        return new Keys(new int[]{n, e}, new int[]{n, d});
    }
    public static void main(String[] args) {
        seed = 1;
        keys = generate_key(8);
        pub = ((int[])(keys.public_key));
        priv = ((int[])(keys.private_key));
        System.out.println("Public key: (" + _p(_geti(pub, 0)) + ", " + _p(_geti(pub, 1)) + ")");
        System.out.println("Private key: (" + _p(_geti(priv, 0)) + ", " + _p(_geti(priv, 1)) + ")");
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
