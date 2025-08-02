public class Main {
    static String ppmData;
    static java.util.Map<String,Object> img;

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (_runeLen(str) > 0 && (str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("0", 0), java.util.Map.entry("1", 1), java.util.Map.entry("2", 2), java.util.Map.entry("3", 3), java.util.Map.entry("4", 4), java.util.Map.entry("5", 5), java.util.Map.entry("6", 6), java.util.Map.entry("7", 7), java.util.Map.entry("8", 8), java.util.Map.entry("9", 9)))));
        while (i < _runeLen(str)) {
            n = n * 10 + (int)(((int)(digits).get(str.substring(i, i + 1))));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static String[] splitWs(String s) {
        String[] parts = new String[]{};
        String cur = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            if ((ch.equals(" ")) || (ch.equals("\n")) || (ch.equals("\t")) || (ch.equals("\r"))) {
                if (_runeLen(cur) > 0) {
                    parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + ch;
            }
            i_1 = i_1 + 1;
        }
        if (_runeLen(cur) > 0) {
            parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        return parts;
    }

    static java.util.Map<String,Object> parsePpm(String data) {
        String[] toks = splitWs(data);
        if (toks.length < 4) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("err", true)));
        }
        String magic = toks[0];
        int w = Integer.parseInt(toks[1]);
        int h = Integer.parseInt(toks[2]);
        int maxv = Integer.parseInt(toks[3]);
        int[] px = new int[]{};
        int i_2 = 4;
        while (i_2 < toks.length) {
            px = java.util.stream.IntStream.concat(java.util.Arrays.stream(px), java.util.stream.IntStream.of(Integer.parseInt(toks[i_2]))).toArray();
            i_2 = i_2 + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("magic", magic), java.util.Map.entry("w", w), java.util.Map.entry("h", h), java.util.Map.entry("max", maxv), java.util.Map.entry("px", px)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ppmData = "P3\n2 2\n1\n0 1 1 0 1 0 0 1 1 1 0 0\n";
            img = parsePpm(ppmData);
            System.out.println("width=" + _p(((int) (img.get("w")))) + " height=" + _p(((int) (img.get("h")))));
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
