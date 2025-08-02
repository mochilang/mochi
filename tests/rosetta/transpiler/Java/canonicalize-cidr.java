public class Main {
    static String[] tests;

    static String[] split(String s, String sep) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (_runeLen(sep) > 0 && i + _runeLen(sep) <= _runeLen(s) && (_substr(s, i, i + _runeLen(sep)).equals(sep))) {
                parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + _runeLen(sep);
            } else {
                cur = cur + s.substring(i, i + 1);
                i = i + 1;
            }
        }
        parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return parts;
    }

    static String join(String[] xs, String sep) {
        String res = "";
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 > 0) {
                res = res + sep;
            }
            res = res + xs[i_1];
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String repeat(String ch, int n) {
        String out = "";
        int i_2 = 0;
        while (i_2 < n) {
            out = out + ch;
            i_2 = i_2 + 1;
        }
        return out;
    }

    static int parseIntStr(String str) {
        int i_3 = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i_3 = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i_3 < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).get(str.substring(i_3, i_3 + 1))));
            i_3 = i_3 + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static String toBinary(int n, int bits) {
        String b = "";
        int val = n;
        int i_4 = 0;
        while (i_4 < bits) {
            b = _p(Math.floorMod(val, 2)) + b;
            val = ((Number)((val / 2))).intValue();
            i_4 = i_4 + 1;
        }
        return b;
    }

    static int binToInt(String bits) {
        int n_1 = 0;
        int i_5 = 0;
        while (i_5 < _runeLen(bits)) {
            n_1 = n_1 * 2 + Integer.parseInt(bits.substring(i_5, i_5 + 1));
            i_5 = i_5 + 1;
        }
        return n_1;
    }

    static String padRight(String s, int width) {
        String out_1 = s;
        while (_runeLen(out_1) < width) {
            out_1 = out_1 + " ";
        }
        return out_1;
    }

    static String canonicalize(String cidr) {
        String[] parts_1 = cidr.split(java.util.regex.Pattern.quote("/"));
        String dotted = parts_1[0];
        int size = Integer.parseInt(parts_1[1]);
        String[] binParts = new String[]{};
        for (var p : dotted.split(java.util.regex.Pattern.quote("."))) {
            binParts = java.util.stream.Stream.concat(java.util.Arrays.stream(binParts), java.util.stream.Stream.of(toBinary(Integer.parseInt(p), 8))).toArray(String[]::new);
        }
        String binary = String.valueOf(join(binParts, ""));
        binary = binary.substring(0, size) + (String)(_repeat("0", 32 - size));
        String[] canonParts = new String[]{};
        int i_6 = 0;
        while (i_6 < _runeLen(binary)) {
            canonParts = java.util.stream.Stream.concat(java.util.Arrays.stream(canonParts), java.util.stream.Stream.of(_p(binToInt(binary.substring(i_6, i_6 + 8))))).toArray(String[]::new);
            i_6 = i_6 + 8;
        }
        return String.valueOf(join(canonParts, ".")) + "/" + parts_1[1];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tests = new String[]{"87.70.141.1/22", "36.18.154.103/12", "62.62.197.11/29", "67.137.119.181/4", "161.214.74.21/24", "184.232.176.184/18"};
            for (String t : tests) {
                System.out.println(String.valueOf(padRight(t, 18)) + " -> " + String.valueOf(canonicalize(t)));
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

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
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
