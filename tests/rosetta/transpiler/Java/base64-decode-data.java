public class Main {
    static String msg = "Rosetta Code Base64 decode data task";
    static String enc = String.valueOf(base64Encode(msg));
    static String dec = String.valueOf(base64Decode(enc));

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < s.length()) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && str.substring(0, 0+1) == "-") {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)));
        while (i < str.length()) {
            n = n * 10 + (int)(((int)digits.getOrDefault(str.substring(i, i+1), 0)));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static int ord(String ch) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        int idx = indexOf(upper, ch);
        if (idx >= 0) {
            return 65 + idx;
        }
        idx = indexOf(lower, ch);
        if (idx >= 0) {
            return 97 + idx;
        }
        if (((ch.compareTo("0") >= 0) && ch.compareTo("9") <= 0)) {
            return 48 + parseIntStr(ch);
        }
        if ((ch.equals("+"))) {
            return 43;
        }
        if ((ch.equals("/"))) {
            return 47;
        }
        if ((ch.equals(" "))) {
            return 32;
        }
        if ((ch.equals("="))) {
            return 61;
        }
        return 0;
    }

    static String chr(int n) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower.substring(n - 97, n - 96);
        }
        if (n >= 48 && n < 58) {
            String digits = "0123456789";
            return digits.substring(n - 48, n - 47);
        }
        if (n == 43) {
            return "+";
        }
        if (n == 47) {
            return "/";
        }
        if (n == 32) {
            return " ";
        }
        if (n == 61) {
            return "=";
        }
        return "?";
    }

    static String toBinary(int n, int bits) {
        String b = "";
        int val = n;
        int i = 0;
        while (i < bits) {
            b = String.valueOf(String.valueOf(Math.floorMod(val, 2)) + b);
            val = ((Number)((val / 2))).intValue();
            i = i + 1;
        }
        return b;
    }

    static int binToInt(String bits) {
        int n = 0;
        int i = 0;
        while (i < bits.length()) {
            n = n * 2 + parseIntStr(bits.substring(i, i + 1));
            i = i + 1;
        }
        return n;
    }

    static String base64Encode(String text) {
        String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        String bin = "";
        for (int _i = 0; _i < text.length(); _i++) {
            var ch = text.substring(_i, _i + 1);
            bin = String.valueOf(bin + String.valueOf(toBinary(ord(String.valueOf(ch)), 8)));
        }
        while (((Number)(Math.floorMod(bin.length(), 6))).intValue() != 0) {
            bin = String.valueOf(bin + "0");
        }
        String out = "";
        int i = 0;
        while (i < bin.length()) {
            String chunk = bin.substring(i, i + 6);
            int val = binToInt(chunk);
            out = String.valueOf(out + alphabet.substring(val, val + 1));
            i = i + 6;
        }
        int pad = Math.floorMod((3 - ((Number)((Math.floorMod(text.length(), 3)))).intValue()), 3);
        if (pad == 1) {
            out = String.valueOf(out.substring(0, out.length() - 1) + "=");
        }
        if (pad == 2) {
            out = String.valueOf(out.substring(0, out.length() - 2) + "==");
        }
        return out;
    }

    static String base64Decode(String enc) {
        String alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        String bin = "";
        int i = 0;
        while (i < enc.length()) {
            String ch = enc.substring(i, i+1);
            if ((ch.equals("="))) {
                break;
            }
            int idx = indexOf(alphabet, ch);
            bin = String.valueOf(bin + String.valueOf(toBinary(idx, 6)));
            i = i + 1;
        }
        String out = "";
        i = 0;
        while (i + 8 <= bin.length()) {
            String chunk = bin.substring(i, i + 8);
            int val = binToInt(chunk);
            out = String.valueOf(out + String.valueOf(chr(val)));
            i = i + 8;
        }
        return out;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("Original : " + msg);
            System.out.println("\nEncoded  : " + enc);
            System.out.println("\nDecoded  : " + dec);
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
}
