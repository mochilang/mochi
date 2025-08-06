public class Main {
    static int seed = 0;
    static int PRIME;
    static int generator;
    static int alice_private;
    static int alice_public;
    static int bob_private;
    static int bob_public;
    static int alice_shared;
    static int bob_shared;

    static String int_to_hex(int n) {
        if (n == 0) {
            return "0";
        }
        String digits = "0123456789abcdef";
        int num = n;
        String res = "";
        while (num > 0) {
            int d = Math.floorMod(num, 16);
            res = digits.substring(d, d+1) + res;
            num = num / 16;
        }
        return res;
    }

    static int rand_int() {
        seed = ((int)(Math.floorMod(((long)((1103515245 * seed + 12345))), 2147483648L)));
        return seed;
    }

    static int mod_pow(int base, int exp) {
        int result = 1;
        int b = Math.floorMod(base, PRIME);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), PRIME);
            }
            b = Math.floorMod((b * b), PRIME);
            e = e / 2;
        }
        return result;
    }

    static boolean is_valid_public_key(int key) {
        if (key < 2 || key > PRIME - 2) {
            return false;
        }
        return mod_pow(key, (PRIME - 1) / 2) == 1;
    }

    static int generate_private_key() {
        return Math.floorMod(rand_int(), (PRIME - 2)) + 2;
    }
    public static void main(String[] args) {
        seed = 123456789;
        PRIME = 23;
        generator = 5;
        alice_private = generate_private_key();
        alice_public = mod_pow(generator, alice_private);
        bob_private = generate_private_key();
        bob_public = mod_pow(generator, bob_private);
        if (!(Boolean)is_valid_public_key(alice_public)) {
            throw new RuntimeException(String.valueOf("Invalid public key"));
        }
        if (!(Boolean)is_valid_public_key(bob_public)) {
            throw new RuntimeException(String.valueOf("Invalid public key"));
        }
        alice_shared = mod_pow(bob_public, alice_private);
        bob_shared = mod_pow(alice_public, bob_private);
        System.out.println(int_to_hex(alice_shared));
        System.out.println(int_to_hex(bob_shared));
    }
}
