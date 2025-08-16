public class Main {
    static String LOWER = "abcdefghijklmnopqrstuvwxyz";
    static String UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    static String PUNCT = "!\"#$%&'()*+,-./:;<=>?@[\\]^_{|}~";
    static String corpus = "This is the first document in the corpus.\nThIs is the second document in the corpus.\nTHIS is the third document in the corpus.";
    static double idf_val;

    static String to_lowercase(String s) {
        String res = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(s))) {
            String c_1 = s.substring((int)((long)(i_1)), (int)((long)(i_1))+1);
            long j_1 = 0L;
            boolean found_1 = false;
            while ((long)(j_1) < (long)(_runeLen(UPPER))) {
                if ((c_1.equals(UPPER.substring((int)((long)(j_1)), (int)((long)(j_1))+1)))) {
                    res = res + LOWER.substring((int)((long)(j_1)), (int)((long)(j_1))+1);
                    found_1 = true;
                    break;
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            if (!found_1) {
                res = res + c_1;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static boolean is_punct(String c) {
        long i_2 = 0L;
        while ((long)(i_2) < (long)(_runeLen(PUNCT))) {
            if ((c.equals(PUNCT.substring((int)((long)(i_2)), (int)((long)(i_2))+1)))) {
                return true;
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        return false;
    }

    static String clean_text(String text, boolean keep_newlines) {
        String lower = String.valueOf(to_lowercase(text));
        String res_2 = "";
        long i_4 = 0L;
        while ((long)(i_4) < (long)(_runeLen(lower))) {
            String ch_1 = lower.substring((int)((long)(i_4)), (int)((long)(i_4))+1);
            if (is_punct(ch_1)) {
            } else             if ((ch_1.equals("\n"))) {
                if (keep_newlines) {
                    res_2 = res_2 + "\n";
                }
            } else {
                res_2 = res_2 + ch_1;
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        return res_2;
    }

    static String[] split(String s, String sep) {
        String[] res_3 = ((String[])(new String[]{}));
        String current_1 = "";
        long i_6 = 0L;
        while ((long)(i_6) < (long)(_runeLen(s))) {
            String ch_3 = s.substring((int)((long)(i_6)), (int)((long)(i_6))+1);
            if ((ch_3.equals(sep))) {
                res_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + ch_3;
            }
            i_6 = (long)((long)(i_6) + 1L);
        }
        res_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_3), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res_3;
    }

    static boolean contains(String s, String sub) {
        long n = (long)(_runeLen(s));
        long m_1 = (long)(_runeLen(sub));
        if ((long)(m_1) == 0L) {
            return true;
        }
        long i_8 = 0L;
        while ((long)(i_8) <= (long)((long)(n) - (long)(m_1))) {
            long j_3 = 0L;
            boolean is_match_1 = true;
            while ((long)(j_3) < (long)(m_1)) {
                if (!(s.substring((int)((long)((long)(i_8) + (long)(j_3))), (int)((long)((long)(i_8) + (long)(j_3)))+1).equals(sub.substring((int)((long)(j_3)), (int)((long)(j_3))+1)))) {
                    is_match_1 = false;
                    break;
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            if (is_match_1) {
                return true;
            }
            i_8 = (long)((long)(i_8) + 1L);
        }
        return false;
    }

    static double floor(double x) {
        long i_9 = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i_9)).doubleValue())) > (double)(x)) {
            i_9 = (long)((long)(i_9) - 1L);
        }
        return ((Number)(i_9)).doubleValue();
    }

    static double round3(double x) {
        return Math.floor((double)((double)(x) * (double)(1000.0)) + (double)(0.5)) / (double)(1000.0);
    }

    static double ln(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_1 = (double)(t);
        double sum_1 = (double)(0.0);
        long k_1 = 1L;
        while ((long)(k_1) <= 99L) {
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)((((Number)(k_1)).doubleValue()))));
            term_1 = (double)((double)((double)(term_1) * (double)(t)) * (double)(t));
            k_1 = (long)((long)(k_1) + 2L);
        }
        return (double)(2.0) * (double)(sum_1);
    }

    static double log10(double x) {
        return (double)(ln((double)(x))) / (double)(ln((double)(10.0)));
    }

    static long term_frequency(String term, String document) {
        String clean = String.valueOf(clean_text(document, false));
        String[] tokens_1 = ((String[])(clean.split(java.util.regex.Pattern.quote(" "))));
        String t_2 = String.valueOf(to_lowercase(term));
        long count_1 = 0L;
        long i_11 = 0L;
        while ((long)(i_11) < (long)(tokens_1.length)) {
            if (!(tokens_1[(int)((long)(i_11))].equals("")) && (tokens_1[(int)((long)(i_11))].equals(t_2))) {
                count_1 = (long)((long)(count_1) + 1L);
            }
            i_11 = (long)((long)(i_11) + 1L);
        }
        return count_1;
    }

    static long[] document_frequency(String term, String corpus) {
        String clean_1 = String.valueOf(clean_text(corpus, true));
        String[] docs_1 = ((String[])(clean_1.split(java.util.regex.Pattern.quote("\n"))));
        String t_4 = String.valueOf(to_lowercase(term));
        long matches_1 = 0L;
        long i_13 = 0L;
        while ((long)(i_13) < (long)(docs_1.length)) {
            if (contains(docs_1[(int)((long)(i_13))], t_4)) {
                matches_1 = (long)((long)(matches_1) + 1L);
            }
            i_13 = (long)((long)(i_13) + 1L);
        }
        return new long[]{matches_1, docs_1.length};
    }

    static double inverse_document_frequency(long df, long n, boolean smoothing) {
        if (smoothing) {
            if ((long)(n) == 0L) {
                throw new RuntimeException(String.valueOf("log10(0) is undefined."));
            }
            double ratio = (double)((double)((((Number)(n)).doubleValue())) / (double)(((double)(1.0) + (double)((((Number)(df)).doubleValue())))));
            double l = (double)(log10((double)(ratio)));
            double result = (double)(round3((double)((double)(1.0) + (double)(l))));
            System.out.println(result);
            return result;
        }
        if ((long)(df) == 0L) {
            throw new RuntimeException(String.valueOf("df must be > 0"));
        }
        if ((long)(n) == 0L) {
            throw new RuntimeException(String.valueOf("log10(0) is undefined."));
        }
        double ratio_2 = (double)((double)((((Number)(n)).doubleValue())) / (double)((((Number)(df)).doubleValue())));
        double l_2 = (double)(log10((double)(ratio_2)));
        double result_2 = (double)(round3((double)(l_2)));
        System.out.println(result_2);
        return result_2;
    }

    static double tf_idf(long tf, double idf) {
        double prod = (double)((double)((((Number)(tf)).doubleValue())) * (double)(idf));
        double result_4 = (double)(round3((double)(prod)));
        System.out.println(result_4);
        return result_4;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(term_frequency("to", "To be, or not to be"));
            System.out.println(_p(document_frequency("first", corpus)));
            idf_val = (double)(inverse_document_frequency(1L, 3L, false));
            tf_idf(2L, (double)(idf_val));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
