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
        double k = (double)(0.0);
        double v_1 = (double)(x);
        while ((double)(v_1) >= (double)(2.0)) {
            v_1 = (double)((double)(v_1) / (double)(2.0));
            k = (double)((double)(k) + (double)(1.0));
        }
        while ((double)(v_1) < (double)(1.0)) {
            v_1 = (double)((double)(v_1) * (double)(2.0));
            k = (double)((double)(k) - (double)(1.0));
        }
        double z_1 = (double)((double)(((double)(v_1) - (double)(1.0))) / (double)(((double)(v_1) + (double)(1.0))));
        double zpow_1 = (double)(z_1);
        double sum_1 = (double)(z_1);
        long i_1 = 3L;
        while ((long)(i_1) <= 9L) {
            zpow_1 = (double)((double)((double)(zpow_1) * (double)(z_1)) * (double)(z_1));
            sum_1 = (double)((double)(sum_1) + (double)((double)(zpow_1) / (double)((((Number)(i_1)).doubleValue()))));
            i_1 = (long)((long)(i_1) + 2L);
        }
        double ln2_1 = (double)(0.6931471805599453);
        return (double)(k) + (double)((double)((double)(2.0) * (double)(sum_1)) / (double)(ln2_1));
    }

    static TextCounts analyze_text(String text) {
        java.util.Map<String,Long> single = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        java.util.Map<String,Long> double_1 = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        long n_1 = (long)(_runeLen(text));
        if ((long)(n_1) == 0L) {
            return new TextCounts(single, double_1);
        }
        String last_1 = _substr(text, (int)((long)((long)(n_1) - 1L)), (int)((long)(n_1)));
        if (single.containsKey(last_1)) {
single.put(last_1, (long)((long)(((long)(single).getOrDefault(last_1, 0L))) + 1L));
        } else {
single.put(last_1, 1L);
        }
        String first_1 = _substr(text, (int)(0L), (int)(1L));
        String pair0_1 = " " + first_1;
double_1.put(pair0_1, 1L);
        long i_3 = 0L;
        while ((long)(i_3) < (long)((long)(n_1) - 1L)) {
            String ch_1 = _substr(text, (int)((long)(i_3)), (int)((long)((long)(i_3) + 1L)));
            if (single.containsKey(ch_1)) {
single.put(ch_1, (long)((long)(((long)(single).getOrDefault(ch_1, 0L))) + 1L));
            } else {
single.put(ch_1, 1L);
            }
            String seq_1 = _substr(text, (int)((long)(i_3)), (int)((long)((long)(i_3) + 2L)));
            if (double_1.containsKey(seq_1)) {
double_1.put(seq_1, (long)((long)(((long)(double_1).getOrDefault(seq_1, 0L))) + 1L));
            } else {
double_1.put(seq_1, 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return new TextCounts(single, double_1);
    }

    static long round_to_int(double x) {
        if ((double)(x) < (double)(0.0)) {
            return ((Number)(((double)(x) - (double)(0.5)))).intValue();
        }
        return ((Number)(((double)(x) + (double)(0.5)))).intValue();
    }

    static void calculate_entropy(String text) {
        TextCounts counts = analyze_text(text);
        String alphas_1 = " abcdefghijklmnopqrstuvwxyz";
        long total1_1 = 0L;
        for (String ch : counts.single.keySet()) {
            total1_1 = (long)((long)(total1_1) + (long)(((long)(counts.single).getOrDefault(ch, 0L))));
        }
        double h1_1 = (double)(0.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(_runeLen(alphas_1))) {
            String ch_3 = _substr(alphas_1, (int)((long)(i_5)), (int)((long)((long)(i_5) + 1L)));
            if (counts.single.containsKey(ch_3)) {
                double prob_1 = (double)((double)((((double)(counts.single).getOrDefault(ch_3, 0L)))) / (double)((((Number)(total1_1)).doubleValue())));
                h1_1 = (double)((double)(h1_1) + (double)((double)(prob_1) * (double)(log2((double)(prob_1)))));
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        double first_entropy_1 = (double)(-h1_1);
        System.out.println(_p(round_to_int((double)(first_entropy_1))) + ".0");
        long total2_1 = 0L;
        for (String seq : counts.double_.keySet()) {
            total2_1 = (long)((long)(total2_1) + (long)(((long)(counts.double_).getOrDefault(seq, 0L))));
        }
        double h2_1 = (double)(0.0);
        long a0_1 = 0L;
        while ((long)(a0_1) < (long)(_runeLen(alphas_1))) {
            String ch0_1 = _substr(alphas_1, (int)((long)(a0_1)), (int)((long)((long)(a0_1) + 1L)));
            long a1_1 = 0L;
            while ((long)(a1_1) < (long)(_runeLen(alphas_1))) {
                String ch1_1 = _substr(alphas_1, (int)((long)(a1_1)), (int)((long)((long)(a1_1) + 1L)));
                String seq_3 = ch0_1 + ch1_1;
                if (counts.double_.containsKey(seq_3)) {
                    double prob_3 = (double)((double)((((double)(counts.double_).getOrDefault(seq_3, 0L)))) / (double)((((Number)(total2_1)).doubleValue())));
                    h2_1 = (double)((double)(h2_1) + (double)((double)(prob_3) * (double)(log2((double)(prob_3)))));
                }
                a1_1 = (long)((long)(a1_1) + 1L);
            }
            a0_1 = (long)((long)(a0_1) + 1L);
        }
        double second_entropy_1 = (double)(-h2_1);
        System.out.println(_p(round_to_int((double)(second_entropy_1))) + ".0");
        double diff_1 = (double)((double)(second_entropy_1) - (double)(first_entropy_1));
        System.out.println(_p(round_to_int((double)(diff_1))) + ".0");
    }
    public static void main(String[] args) {
        text1 = "Behind Winston's back the voice " + "from the telescreen was still " + "babbling and the overfulfilment";
        calculate_entropy(text1);
        text3 = "Had repulsive dashwoods suspicion sincerity but advantage now him. " + "Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. " + "You greatest jointure saw horrible. He private he on be imagine " + "suppose. Fertile beloved evident through no service elderly is. Blind " + "there if every no so at. Own neglected you preferred way sincerity " + "delivered his attempted. To of message cottage windows do besides " + "against uncivil.  Delightful unreserved impossible few estimating " + "men favourable see entreaties. She propriety immediate was improving. " + "He or entrance humoured likewise moderate. Much nor game son say " + "feel. Fat make met can must form into gate. Me we offending prevailed " + "discovery.";
        calculate_entropy(text3);
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
