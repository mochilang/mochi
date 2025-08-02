public class Main {

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = ((Number)(upper.indexOf(ch))).intValue();
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = ((Number)(lower.indexOf(ch))).intValue();
        if (idx >= 0) {
            return 97 + idx;
        }
        return 0;
    }

    static String chr(int n) {
        String upper_1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower_1 = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper_1.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower_1.substring(n - 97, n - 96);
        }
        return "?";
    }

    static String shiftRune(String r, int k) {
        if ((r.compareTo("a") >= 0) && (r.compareTo("z") <= 0)) {
            return chr((Math.floorMod((ord(r) - 97 + k), 26)) + 97);
        }
        if ((r.compareTo("A") >= 0) && (r.compareTo("Z") <= 0)) {
            return chr((Math.floorMod((ord(r) - 65 + k), 26)) + 65);
        }
        return r;
    }

    static String encipher(String s, int k) {
        String out = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            out = out + String.valueOf(shiftRune(s.substring(i_1, i_1 + 1), k));
            i_1 = i_1 + 1;
        }
        return out;
    }

    static String decipher(String s, int k) {
        return encipher(s, Math.floorMod((26 - Math.floorMod(k, 26)), 26));
    }

    static void main() {
        String pt = "The five boxing wizards jump quickly";
        System.out.println("Plaintext: " + pt);
        for (int key : new int[]{0, 1, 7, 25, 26}) {
            if (key < 1 || key > 25) {
                System.out.println("Key " + _p(key) + " invalid");
                continue;
            }
            String ct = String.valueOf(encipher(pt, key));
            System.out.println("Key " + _p(key));
            System.out.println("  Enciphered: " + ct);
            System.out.println("  Deciphered: " + String.valueOf(decipher(ct, key)));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
