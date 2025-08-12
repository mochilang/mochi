public class Main {
    static class TextCounts {
        java.util.Map<String,Long> single;
        java.util.Map<String,Long> double_;
        TextCounts(java.util.Map<String,Long> single, java.util.Map<String,Long> double_) {
            this.single = single;
            this.double_ = double_;
        }
        TextCounts() {}
        @Override public String toString() {
            return String.format("{'single': %s, 'double': %s}", String.valueOf(single), String.valueOf(double_));
        }
    }

    static String text1;
    static String text3;

    static double log2(double x) {
        double k = 0.0;
        double v_1 = x;
        while (v_1 >= 2.0) {
            v_1 = v_1 / 2.0;
            k = k + 1.0;
        }
        while (v_1 < 1.0) {
            v_1 = v_1 * 2.0;
            k = k - 1.0;
        }
        double z_1 = (v_1 - 1.0) / (v_1 + 1.0);
        double zpow_1 = z_1;
        double sum_1 = z_1;
        long i_1 = 3L;
        while (i_1 <= 9) {
            zpow_1 = zpow_1 * z_1 * z_1;
            sum_1 = sum_1 + zpow_1 / (((Number)(i_1)).doubleValue());
            i_1 = i_1 + 2;
        }
        double ln2_1 = 0.6931471805599453;
        return k + 2.0 * sum_1 / ln2_1;
    }

    static TextCounts analyze_text(String text) {
        java.util.Map<String,Long> single = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        java.util.Map<String,Long> double_1 = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        long n_1 = _runeLen(text);
        if (n_1 == 0) {
            return new TextCounts(single, double_1);
        }
        String last_1 = _substr(text, (int)(n_1 - 1), (int)(n_1));
        if (single.containsKey(last_1)) {
single.put(last_1, (long)(((long)(single).getOrDefault(last_1, 0L))) + 1);
        } else {
single.put(last_1, 1L);
        }
        String first_1 = _substr(text, (int)(0), (int)(1));
        String pair0_1 = " " + first_1;
double_1.put(pair0_1, 1L);
        long i_3 = 0L;
        while (i_3 < n_1 - 1) {
            String ch_1 = _substr(text, (int)(i_3), (int)(i_3 + 1));
            if (single.containsKey(ch_1)) {
single.put(ch_1, (long)(((long)(single).getOrDefault(ch_1, 0L))) + 1);
            } else {
single.put(ch_1, 1L);
            }
            String seq_1 = _substr(text, (int)(i_3), (int)(i_3 + 2));
            if (double_1.containsKey(seq_1)) {
double_1.put(seq_1, (long)(((long)(double_1).getOrDefault(seq_1, 0L))) + 1);
            } else {
double_1.put(seq_1, 1L);
            }
            i_3 = i_3 + 1;
        }
        return new TextCounts(single, double_1);
    }

    static long round_to_int(double x) {
        if (x < 0.0) {
            return ((Number)((x - 0.5))).intValue();
        }
        return ((Number)((x + 0.5))).intValue();
    }

    static void calculate_entropy(String text) {
        TextCounts counts = analyze_text(text);
        String alphas_1 = " abcdefghijklmnopqrstuvwxyz";
        long total1_1 = 0L;
        for (String ch : counts.single.keySet()) {
            total1_1 = total1_1 + (long)(((long)(counts.single).getOrDefault(ch, 0L)));
        }
        double h1_1 = 0.0;
        long i_5 = 0L;
        while (i_5 < _runeLen(alphas_1)) {
            String ch_3 = _substr(alphas_1, (int)(i_5), (int)(i_5 + 1));
            if (counts.single.containsKey(ch_3)) {
                double prob_1 = (((double)(counts.single).getOrDefault(ch_3, 0L))) / (((Number)(total1_1)).doubleValue());
                h1_1 = h1_1 + prob_1 * log2(prob_1);
            }
            i_5 = i_5 + 1;
        }
        double first_entropy_1 = -h1_1;
        System.out.println(_p(round_to_int(first_entropy_1)) + ".0");
        long total2_1 = 0L;
        for (String seq : counts.double_.keySet()) {
            total2_1 = total2_1 + (long)(((long)(counts.double_).getOrDefault(seq, 0L)));
        }
        double h2_1 = 0.0;
        long a0_1 = 0L;
        while (a0_1 < _runeLen(alphas_1)) {
            String ch0_1 = _substr(alphas_1, (int)(a0_1), (int)(a0_1 + 1));
            long a1_1 = 0L;
            while (a1_1 < _runeLen(alphas_1)) {
                String ch1_1 = _substr(alphas_1, (int)(a1_1), (int)(a1_1 + 1));
                String seq_3 = ch0_1 + ch1_1;
                if (counts.double_.containsKey(seq_3)) {
                    double prob_3 = (((double)(counts.double_).getOrDefault(seq_3, 0L))) / (((Number)(total2_1)).doubleValue());
                    h2_1 = h2_1 + prob_3 * log2(prob_3);
                }
                a1_1 = a1_1 + 1;
            }
            a0_1 = a0_1 + 1;
        }
        double second_entropy_1 = -h2_1;
        System.out.println(_p(round_to_int(second_entropy_1)) + ".0");
        double diff_1 = second_entropy_1 - first_entropy_1;
        System.out.println(_p(round_to_int(diff_1)) + ".0");
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
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
