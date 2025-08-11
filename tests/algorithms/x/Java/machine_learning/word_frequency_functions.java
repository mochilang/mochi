public class Main {
    static String LOWER;
    static String UPPER;
    static String PUNCT;
    static String corpus;
    static double idf_val;

    static String to_lowercase(String s) {
        String res = "";
        long i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String c_1 = s.substring((int)(i_1), (int)(i_1)+1);
            long j_1 = 0;
            boolean found_1 = false;
            while (j_1 < _runeLen(UPPER)) {
                if ((c_1.equals(UPPER.substring((int)(j_1), (int)(j_1)+1)))) {
                    res = res + LOWER.substring((int)(j_1), (int)(j_1)+1);
                    found_1 = true;
                    break;
                }
                j_1 = j_1 + 1;
            }
            if (!found_1) {
                res = res + c_1;
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static boolean is_punct(String c) {
        long i_2 = 0;
        while (i_2 < _runeLen(PUNCT)) {
            if ((c.equals(PUNCT.substring((int)(i_2), (int)(i_2)+1)))) {
                return true;
            }
            i_2 = i_2 + 1;
        }
        return false;
    }

    static String clean_text(String text, boolean keep_newlines) {
        String lower = String.valueOf(to_lowercase(text));
        String res_2 = "";
        long i_4 = 0;
        while (i_4 < _runeLen(lower)) {
            String ch_1 = lower.substring((int)(i_4), (int)(i_4)+1);
            if (((Boolean)(is_punct(ch_1)))) {
            } else             if ((ch_1.equals("\n"))) {
                if (((Boolean)(keep_newlines))) {
                    res_2 = res_2 + "\n";
                }
            } else {
                res_2 = res_2 + ch_1;
            }
            i_4 = i_4 + 1;
        }
        return res_2;
    }

    static String[] split(String s, String sep) {
        String[] res_3 = ((String[])(new String[]{}));
        String current_1 = "";
        long i_6 = 0;
        while (i_6 < _runeLen(s)) {
            String ch_3 = s.substring((int)(i_6), (int)(i_6)+1);
            if ((ch_3.equals(sep))) {
                res_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + ch_3;
            }
            i_6 = i_6 + 1;
        }
        res_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res_3;
    }

    static boolean contains(String s, String sub) {
        long n = _runeLen(s);
        long m_1 = _runeLen(sub);
        if (m_1 == 0) {
            return true;
        }
        long i_8 = 0;
        while (i_8 <= n - m_1) {
            long j_3 = 0;
            boolean is_match_1 = true;
            while (j_3 < m_1) {
                if (!(s.substring((int)(i_8 + j_3), (int)(i_8 + j_3)+1).equals(sub.substring((int)(j_3), (int)(j_3)+1)))) {
                    is_match_1 = false;
                    break;
                }
                j_3 = j_3 + 1;
            }
            if (is_match_1) {
                return true;
            }
            i_8 = i_8 + 1;
        }
        return false;
    }

    static double floor(double x) {
        long i_9 = ((Number)(x)).intValue();
        if ((((Number)(i_9)).doubleValue()) > x) {
            i_9 = i_9 - 1;
        }
        return ((Number)(i_9)).doubleValue();
    }

    static double round3(double x) {
        return floor(x * 1000.0 + 0.5) / 1000.0;
    }

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term_1 = t;
        double sum_1 = 0.0;
        long k_1 = 1;
        while (k_1 <= 99) {
            sum_1 = sum_1 + term_1 / (((Number)(k_1)).doubleValue());
            term_1 = term_1 * t * t;
            k_1 = k_1 + 2;
        }
        return 2.0 * sum_1;
    }

    static double log10(double x) {
        return ln(x) / ln(10.0);
    }

    static long term_frequency(String term, String document) {
        String clean = String.valueOf(clean_text(document, false));
        String[] tokens_1 = ((String[])(clean.split(java.util.regex.Pattern.quote(" "))));
        String t_2 = String.valueOf(to_lowercase(term));
        long count_1 = 0;
        long i_11 = 0;
        while (i_11 < tokens_1.length) {
            if (!(tokens_1[(int)(i_11)].equals("")) && (tokens_1[(int)(i_11)].equals(t_2))) {
                count_1 = count_1 + 1;
            }
            i_11 = i_11 + 1;
        }
        return count_1;
    }

    static long[] document_frequency(String term, String corpus) {
        String clean_1 = String.valueOf(clean_text(corpus, true));
        String[] docs_1 = ((String[])(clean_1.split(java.util.regex.Pattern.quote("\n"))));
        String t_4 = String.valueOf(to_lowercase(term));
        long matches_1 = 0;
        long i_13 = 0;
        while (i_13 < docs_1.length) {
            if (((Boolean)(contains(docs_1[(int)(i_13)], t_4)))) {
                matches_1 = matches_1 + 1;
            }
            i_13 = i_13 + 1;
        }
        return new long[]{matches_1, docs_1.length};
    }

    static double inverse_document_frequency(long df, long n, boolean smoothing) {
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
        double ratio_2 = (((Number)(n)).doubleValue()) / (((Number)(df)).doubleValue());
        double l_2 = log10(ratio_2);
        double result_2 = round3(l_2);
        System.out.println(result_2);
        return result_2;
    }

    static double tf_idf(long tf, double idf) {
        double prod = (((Number)(tf)).doubleValue()) * idf;
        double result_4 = round3(prod);
        System.out.println(result_4);
        return result_4;
    }
    public static void main(String[] args) {
        LOWER = "abcdefghijklmnopqrstuvwxyz";
        UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        PUNCT = "!\"#$%&'()*+,-./:;<=>?@[\\]^_{|}~";
        System.out.println(term_frequency("to", "To be, or not to be"));
        corpus = "This is the first document in the corpus.\nThIs is the second document in the corpus.\nTHIS is the third document in the corpus.";
        System.out.println(_p(document_frequency("first", corpus)));
        idf_val = inverse_document_frequency(1, 3, false);
        tf_idf(2, idf_val);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
