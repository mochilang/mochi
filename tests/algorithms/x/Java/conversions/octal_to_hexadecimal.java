public class Main {
    static String[] nums;
    static int t = 0;

    static String octal_to_hex(String octal) {
        String s = octal;
        if (_runeLen(s) >= 2 && (s.substring(0, 0+1).equals("0")) && (s.substring(1, 1+1).equals("o"))) {
            s = _substr(s, 2, _runeLen(s));
        }
        if (_runeLen(s) == 0) {
            throw new RuntimeException(String.valueOf("Empty string was passed to the function"));
        }
        int j = 0;
        while (j < _runeLen(s)) {
            String c = s.substring(j, j+1);
            if (!(c.equals("0")) && !(c.equals("1")) && !(c.equals("2")) && !(c.equals("3")) && !(c.equals("4")) && !(c.equals("5")) && !(c.equals("6")) && !(c.equals("7"))) {
                throw new RuntimeException(String.valueOf("Not a Valid Octal Number"));
            }
            j = j + 1;
        }
        int decimal = 0;
        int k = 0;
        while (k < _runeLen(s)) {
            int d = Integer.parseInt(s.substring(k, k+1));
            decimal = decimal * 8 + d;
            k = k + 1;
        }
        String hex_chars = "0123456789ABCDEF";
        if (decimal == 0) {
            return "0x";
        }
        String hex = "";
        while (decimal > 0) {
            int idx = Math.floorMod(decimal, 16);
            hex = hex_chars.substring(idx, idx+1) + hex;
            decimal = Math.floorDiv(decimal, 16);
        }
        return "0x" + hex;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            nums = ((String[])(new String[]{"030", "100", "247", "235", "007"}));
            t = 0;
            while (t < nums.length) {
                String num = nums[t];
                System.out.println(octal_to_hex(num));
                t = t + 1;
            }
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
}
