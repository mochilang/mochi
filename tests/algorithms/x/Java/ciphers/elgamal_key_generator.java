public class Main {
    static int seed = 0;
    static class GCD {
        int g;
        int x;
        int y;
        GCD(int g, int x, int y) {
            this.g = g;
            this.x = x;
            this.y = y;
        }
        GCD() {}
        @Override public String toString() {
            return String.format("{'g': %s, 'x': %s, 'y': %s}", String.valueOf(g), String.valueOf(x), String.valueOf(y));
        }
    }

    static class PublicKey {
        int key_size;
        int g;
        int e2;
        int p;
        PublicKey(int key_size, int g, int e2, int p) {
            this.key_size = key_size;
            this.g = g;
            this.e2 = e2;
            this.p = p;
        }
        PublicKey() {}
        @Override public String toString() {
            return String.format("{'key_size': %s, 'g': %s, 'e2': %s, 'p': %s}", String.valueOf(key_size), String.valueOf(g), String.valueOf(e2), String.valueOf(p));
        }
    }

    static class PrivateKey {
        int key_size;
        int d;
        PrivateKey(int key_size, int d) {
            this.key_size = key_size;
            this.d = d;
        }
        PrivateKey() {}
        @Override public String toString() {
            return String.format("{'key_size': %s, 'd': %s}", String.valueOf(key_size), String.valueOf(d));
        }
    }

    static class KeyPair {
        PublicKey public_key;
        PrivateKey private_key;
        KeyPair(PublicKey public_key, PrivateKey private_key) {
            this.public_key = public_key;
            this.private_key = private_key;
        }
        KeyPair() {}
        @Override public String toString() {
            return String.format("{'public_key': %s, 'private_key': %s}", String.valueOf(public_key), String.valueOf(private_key));
        }
    }


    static int rand() {
        seed = Math.floorMod((seed * 1103515245 + 12345), 2147483647);
        return seed;
    }

    static int rand_range(int min, int max) {
        return min + Math.floorMod(rand(), (max - min + 1));
    }

    static int mod_pow(int base, int exponent, int modulus) {
        int result = 1;
        int b = Math.floorMod(base, modulus);
        int e = exponent;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), modulus);
            }
            e = e / 2;
            b = Math.floorMod((b * b), modulus);
        }
        return result;
    }

    static GCD extended_gcd(int a, int b) {
        if (b == 0) {
            return new GCD(a, 1, 0);
        }
        GCD res = extended_gcd(b, Math.floorMod(a, b));
        return new GCD(res.g, res.y, res.x - (a / b) * res.y);
    }

    static int mod_inverse(int a, int m) {
        GCD res_1 = extended_gcd(a, m);
        if (res_1.g != 1) {
            throw new RuntimeException(String.valueOf("inverse does not exist"));
        }
        int r = Math.floorMod(res_1.x, m);
        if (r < 0) {
            return r + m;
        }
        return r;
    }

    static int pow2(int n) {
        int r_1 = 1;
        int i = 0;
        while (i < n) {
            r_1 = r_1 * 2;
            i = i + 1;
        }
        return r_1;
    }

    static boolean is_probable_prime(int n, int k) {
        if (n <= 1) {
            return false;
        }
        if (n <= 3) {
            return true;
        }
        if (Math.floorMod(n, 2) == 0) {
            return false;
        }
        int r_2 = 0;
        int d = n - 1;
        while (Math.floorMod(d, 2) == 0) {
            d = d / 2;
            r_2 = r_2 + 1;
        }
        int i_1 = 0;
        while (i_1 < k) {
            int a = rand_range(2, n - 2);
            int x = mod_pow(a, d, n);
            if (x == 1 || x == n - 1) {
                i_1 = i_1 + 1;
                continue;
            }
            int j = 1;
            boolean found = false;
            while (j < r_2) {
                x = mod_pow(x, 2, n);
                if (x == n - 1) {
                    found = true;
                    break;
                }
                j = j + 1;
            }
            if (!found) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static int generate_large_prime(int bits) {
        int min = pow2(bits - 1);
        int max = pow2(bits) - 1;
        int p = rand_range(min, max);
        if (Math.floorMod(p, 2) == 0) {
            p = p + 1;
        }
        while (!(Boolean)is_probable_prime(p, 5)) {
            p = p + 2;
            if (p > max) {
                p = min + 1;
            }
        }
        return p;
    }

    static int primitive_root(int p) {
        while (true) {
            int g = rand_range(3, p - 1);
            if (mod_pow(g, 2, p) == 1) {
                continue;
            }
            if (mod_pow(g, p, p) == 1) {
                continue;
            }
            return g;
        }
    }

    static KeyPair generate_key(int key_size) {
        int p_1 = generate_large_prime(key_size);
        int e1 = primitive_root(p_1);
        int d_1 = rand_range(3, p_1 - 1);
        int e2 = mod_inverse(mod_pow(e1, d_1, p_1), p_1);
        PublicKey public_key = new PublicKey(key_size, e1, e2, p_1);
        PrivateKey private_key = new PrivateKey(key_size, d_1);
        return new KeyPair(public_key, private_key);
    }

    static void main() {
        int key_size = 16;
        KeyPair kp = generate_key(key_size);
        PublicKey pub = kp.public_key;
        PrivateKey priv = kp.private_key;
        System.out.println("public key: (" + _p(pub.key_size) + ", " + _p(pub.g) + ", " + _p(pub.e2) + ", " + _p(pub.p) + ")");
        System.out.println("private key: (" + _p(priv.key_size) + ", " + _p(priv.d) + ")");
    }
    public static void main(String[] args) {
        seed = 123456789;
        main();
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
}
