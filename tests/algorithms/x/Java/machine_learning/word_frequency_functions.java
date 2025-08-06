public class Main {
    static String LOWER;
    static String UPPER;
    static String PUNCT;
    static String corpus;
    static double idf_val;

    static String to_lowercase(String s) {
        String res = "";
        int i = 0;
        while (i < _runeLen(s)) {
            String c = s.substring(i, i+1);
            int j = 0;
            boolean found = false;
            while (j < _runeLen(UPPER)) {
                if ((c.equals(UPPER.substring(j, j+1)))) {
                    res = res + LOWER.substring(j, j+1);
                    found = true;
                    break;
                }
                j = j + 1;
            }
            if (!found) {
                res = res + c;
            }
            i = i + 1;
        }
        return res;
    }

    static boolean is_punct(String c) {
        int i_1 = 0;
        while (i_1 < _runeLen(PUNCT)) {
            if ((c.equals(PUNCT.substring(i_1, i_1+1)))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static String clean_text(String text, boolean keep_newlines) {
        String lower = String.valueOf(to_lowercase(text));
        String res_1 = "";
        int i_2 = 0;
        while (i_2 < _runeLen(lower)) {
            String ch = lower.substring(i_2, i_2+1);
            if (((Boolean)(is_punct(ch)))) {
            } else             if ((ch.equals("\n"))) {
                if (((Boolean)(keep_newlines))) {
                    res_1 = res_1 + "\n";
                }
            } else {
                res_1 = res_1 + ch;
            }
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static String[] split(String s, String sep) {
        String[] res_2 = ((String[])(new String[]{}));
        String current = "";
        int i_3 = 0;
        while (i_3 < _runeLen(s)) {
            String ch_1 = s.substring(i_3, i_3+1);
            if ((ch_1.equals(sep))) {
                res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch_1;
            }
            i_3 = i_3 + 1;
        }
        res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res_2;
    }

    static boolean contains(String s, String sub) {
        int n = _runeLen(s);
        int m = _runeLen(sub);
        if (m == 0) {
            return true;
        }
        int i_4 = 0;
        while (i_4 <= n - m) {
            int j_1 = 0;
            boolean is_match = true;
            while (j_1 < m) {
                if (!(s.substring(i_4 + j_1, i_4 + j_1+1).equals(sub.substring(j_1, j_1+1)))) {
                    is_match = false;
                    break;
                }
                j_1 = j_1 + 1;
            }
            if (is_match) {
                return true;
            }
            i_4 = i_4 + 1;
        }
        return false;
    }

    static double floor(double x) {
        int i_5 = ((Number)(x)).intValue();
        if ((((Number)(i_5)).doubleValue()) > x) {
            i_5 = i_5 - 1;
        }
        return ((Number)(i_5)).doubleValue();
    }

    static double round3(double x) {
        return floor(x * 1000.0 + 0.5) / 1000.0;
    }

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term = t;
        double sum = 0.0;
        int k = 1;
        while (k <= 99) {
            sum = sum + term / (((Number)(k)).doubleValue());
            term = term * t * t;
            k = k + 2;
        }
        return 2.0 * sum;
    }

    static double log10(double x) {
        return ln(x) / ln(10.0);
    }

    static int term_frequency(String term, String document) {
        String clean = String.valueOf(clean_text(document, false));
        String[] tokens = ((String[])(clean.split(java.util.regex.Pattern.quote(" "))));
        String t_1 = String.valueOf(to_lowercase(term));
        int count = 0;
        int i_6 = 0;
        while (i_6 < tokens.length) {
            if (!(tokens[i_6].equals("")) && (tokens[i_6].equals(t_1))) {
                count = count + 1;
            }
            i_6 = i_6 + 1;
        }
        return count;
    }

    static int[] document_frequency(String term, String corpus) {
        String clean_1 = String.valueOf(clean_text(corpus, true));
        String[] docs = ((String[])(clean_1.split(java.util.regex.Pattern.quote("\n"))));
        String t_2 = String.valueOf(to_lowercase(term));
        int matches = 0;
        int i_7 = 0;
        while (i_7 < docs.length) {
            if (((Boolean)(contains(docs[i_7], t_2)))) {
                matches = matches + 1;
            }
            i_7 = i_7 + 1;
        }
        return new int[]{matches, docs.length};
    }

    static double inverse_document_frequency(int df, int n, boolean smoothing) {
        if (((Boolean)(smoothing))) {
            if (n == 0) {
                throw new RuntimeException(String.valueOf("log10(0) is undefined."));
            }
            double ratio = (((Number)(n)).doubleValue()) / (1.0 + (((Number)(df)).doubleValue()));
            double l = log10(ratio);
            double result = round3(1.0 + l);
            System.out.println(result);
            return result;
        }
        if (df == 0) {
            throw new RuntimeException(String.valueOf("df must be > 0"));
        }
        if (n == 0) {
            throw new RuntimeException(String.valueOf("log10(0) is undefined."));
        }
        double ratio_1 = (((Number)(n)).doubleValue()) / (((Number)(df)).doubleValue());
        double l_1 = log10(ratio_1);
        double result_1 = round3(l_1);
        System.out.println(result_1);
        return result_1;
    }

    static double tf_idf(int tf, double idf) {
        double prod = (((Number)(tf)).doubleValue()) * idf;
        double result_2 = round3(prod);
        System.out.println(result_2);
        return result_2;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            LOWER = "abcdefghijklmnopqrstuvwxyz";
            UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
            PUNCT = "!\"#$%&'()*+,-./:;<=>?@[\\]^_{|}~";
            System.out.println(term_frequency("to", "To be, or not to be"));
            corpus = "This is the first document in the corpus.\nThIs is the second document in the corpus.\nTHIS is the third document in the corpus.";
            System.out.println(_p(document_frequency("first", corpus)));
            idf_val = inverse_document_frequency(1, 3, false);
            tf_idf(2, idf_val);
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
