public class Main {

    static double log2(double x) {
        double k = 0.0;
        double v = x;
        while (v >= 2.0) {
            v = v / 2.0;
            k = k + 1.0;
        }
        while (v < 1.0) {
            v = v * 2.0;
            k = k - 1.0;
        }
        double z = (v - 1.0) / (v + 1.0);
        double zpow = z;
        double sum = z;
        int i = 3;
        while (i <= 9) {
            zpow = zpow * z * z;
            sum = sum + zpow / (((Number)(i)).doubleValue());
            i = i + 2;
        }
        double ln2 = 0.6931471805599453;
        return k + 2.0 * sum / ln2;
    }

    static double H(String data) {
        if ((data.equals(""))) {
            return 0.0;
        }
        java.util.Map<String,Integer> counts = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i_1 = 0;
        while (i_1 < _runeLen(data)) {
            String ch = _substr(data, i_1, i_1 + 1);
            if (counts.containsKey(ch)) {
counts.put(ch, (int)(((int)(counts).getOrDefault(ch, 0))) + 1);
            } else {
counts.put(ch, 1);
            }
            i_1 = i_1 + 1;
        }
        double entropy = 0.0;
        double l = ((Number)(_runeLen(data))).doubleValue();
        for (String ch : counts.keySet()) {
            double px = (((double)(counts).getOrDefault(ch, 0))) / l;
            if (px > 0.0) {
                entropy = entropy - px * log2(px);
            }
        }
        return entropy;
    }

    static void main() {
        System.out.println(String.valueOf(H("1223334444")));
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
}
