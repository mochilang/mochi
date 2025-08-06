public class Main {
    static class TextCounts {
        java.util.Map<String,Integer> single;
        java.util.Map<String,Integer> double;
        TextCounts(java.util.Map<String,Integer> single, java.util.Map<String,Integer> double_) {
            this.single = single;
            this.double_ = double_;
        }
        TextCounts() {}
        @Override public String toString() {
            return String.format("{'single': %s, 'double': %s}", String.valueOf(single), String.valueOf(double));
        }
    }

    static String text1;
    static String text3;

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

    static TextCounts analyze_text(String text) {
        java.util.Map<String,Integer> single = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        java.util.Map<String,Integer> double_ = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int n = _runeLen(text);
        if (n == 0) {
            return new TextCounts(single, double_);
        }
        String last = _substr(text, n - 1, n);
        if (((Boolean)(single.containsKey(last)))) {
single.put(last, (int)(((int)(single).getOrDefault(last, 0))) + 1);
        } else {
single.put(last, 1);
        }
        String first = _substr(text, 0, 1);
        String pair0 = " " + first;
double_.put(pair0, 1);
        int i_1 = 0;
        while (i_1 < n - 1) {
            String ch = _substr(text, i_1, i_1 + 1);
            if (((Boolean)(single.containsKey(ch)))) {
single.put(ch, (int)(((int)(single).getOrDefault(ch, 0))) + 1);
            } else {
single.put(ch, 1);
            }
            String seq = _substr(text, i_1, i_1 + 2);
            if (((Boolean)(double_.containsKey(seq)))) {
double_.put(seq, (int)(((int)(double_).getOrDefault(seq, 0))) + 1);
            } else {
double_.put(seq, 1);
            }
            i_1 = i_1 + 1;
        }
        return new TextCounts(single, double_);
    }

    static int round_to_int(double x) {
        if (x < 0.0) {
            return ((Number)((x - 0.5))).intValue();
        }
        return ((Number)((x + 0.5))).intValue();
    }

    static void calculate_entropy(String text) {
        TextCounts counts = analyze_text(text);
        String alphas = " abcdefghijklmnopqrstuvwxyz";
        int total1 = 0;
        for (String ch : counts.single.keySet()) {
            total1 = total1 + (int)(((int)(counts.single).getOrDefault(ch, 0)));
        }
        double h1 = 0.0;
        int i_2 = 0;
        while (i_2 < _runeLen(alphas)) {
            String ch_1 = _substr(alphas, i_2, i_2 + 1);
            if (((Boolean)(counts.single.containsKey(ch_1)))) {
                double prob = (((double)(counts.single).getOrDefault(ch_1, 0))) / (((Number)(total1)).doubleValue());
                h1 = h1 + prob * log2(prob);
            }
            i_2 = i_2 + 1;
        }
        double first_entropy = -h1;
        System.out.println(_p(round_to_int(first_entropy)) + ".0");
        int total2 = 0;
        for (String seq : counts.double.keySet()) {
            total2 = total2 + (int)(((int)(counts.double).getOrDefault(seq, 0)));
        }
        double h2 = 0.0;
        int a0 = 0;
        while (a0 < _runeLen(alphas)) {
            String ch0 = _substr(alphas, a0, a0 + 1);
            int a1 = 0;
            while (a1 < _runeLen(alphas)) {
                String ch1 = _substr(alphas, a1, a1 + 1);
                String seq_1 = ch0 + ch1;
                if (((Boolean)(counts.double.containsKey(seq_1)))) {
                    double prob_1 = (((double)(counts.double).getOrDefault(seq_1, 0))) / (((Number)(total2)).doubleValue());
                    h2 = h2 + prob_1 * log2(prob_1);
                }
                a1 = a1 + 1;
            }
            a0 = a0 + 1;
        }
        double second_entropy = -h2;
        System.out.println(_p(round_to_int(second_entropy)) + ".0");
        double diff = second_entropy - first_entropy;
        System.out.println(_p(round_to_int(diff)) + ".0");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            text1 = "Behind Winston's back the voice " + "from the telescreen was still " + "babbling and the overfulfilment";
            calculate_entropy(text1);
            text3 = "Had repulsive dashwoods suspicion sincerity but advantage now him. " + "Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. " + "You greatest jointure saw horrible. He private he on be imagine " + "suppose. Fertile beloved evident through no service elderly is. Blind " + "there if every no so at. Own neglected you preferred way sincerity " + "delivered his attempted. To of message cottage windows do besides " + "against uncivil.  Delightful unreserved impossible few estimating " + "men favourable see entreaties. She propriety immediate was improving. " + "He or entrance humoured likewise moderate. Much nor game son say " + "feel. Fat make met can must form into gate. Me we offending prevailed " + "discovery.";
            calculate_entropy(text3);
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
