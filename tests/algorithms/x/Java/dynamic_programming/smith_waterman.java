public class Main {
    static String query = "HEAGAWGHEE";
    static String subject = "PAWHEAE";
    static java.math.BigInteger[][] score_2;

    static java.math.BigInteger score_function(String source_char, String target_char, java.math.BigInteger match_score, java.math.BigInteger mismatch_score, java.math.BigInteger gap_score) {
        if ((source_char.equals("-")) || (target_char.equals("-"))) {
            return new java.math.BigInteger(String.valueOf(gap_score));
        }
        if ((source_char.equals(target_char))) {
            return new java.math.BigInteger(String.valueOf(match_score));
        }
        return new java.math.BigInteger(String.valueOf(mismatch_score));
    }

    static java.math.BigInteger[][] smith_waterman(String query, String subject, java.math.BigInteger match_score, java.math.BigInteger mismatch_score, java.math.BigInteger gap_score) {
        String q = query.toUpperCase();
        String s_1 = subject.toUpperCase();
        java.math.BigInteger m_1 = new java.math.BigInteger(String.valueOf(_runeLen(q)));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(_runeLen(s_1)));
        java.math.BigInteger[][] score_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        for (java.math.BigInteger _v = new java.math.BigInteger(String.valueOf(0)); _v.compareTo((m_1.add(java.math.BigInteger.valueOf(1)))) < 0; _v = _v.add(java.math.BigInteger.ONE)) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            for (java.math.BigInteger _2 = new java.math.BigInteger(String.valueOf(0)); _2.compareTo((n_1.add(java.math.BigInteger.valueOf(1)))) < 0; _2 = _2.add(java.math.BigInteger.ONE)) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
            }
            score_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(score_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
        }
        for (java.math.BigInteger i = new java.math.BigInteger(String.valueOf(1)); i.compareTo((m_1.add(java.math.BigInteger.valueOf(1)))) < 0; i = i.add(java.math.BigInteger.ONE)) {
            for (java.math.BigInteger j = new java.math.BigInteger(String.valueOf(1)); j.compareTo((n_1.add(java.math.BigInteger.valueOf(1)))) < 0; j = j.add(java.math.BigInteger.ONE)) {
                String qc_1 = _substr(q, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(i)).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(i)));
                String sc_1 = _substr(s_1, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(j)).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(j)));
                java.math.BigInteger diag_1 = new java.math.BigInteger(String.valueOf(score_1[_idx((score_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(i)).subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((score_1[_idx((score_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(i)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(j)).subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(score_function(qc_1, sc_1, new java.math.BigInteger(String.valueOf(match_score)), new java.math.BigInteger(String.valueOf(mismatch_score)), new java.math.BigInteger(String.valueOf(gap_score))))));
                java.math.BigInteger delete_1 = new java.math.BigInteger(String.valueOf(score_1[_idx((score_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(i)).subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((score_1[_idx((score_1).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(i)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, (long)(j))].add(gap_score)));
                java.math.BigInteger insert_1 = new java.math.BigInteger(String.valueOf(score_1[_idx((score_1).length, (long)(i))][_idx((score_1[_idx((score_1).length, (long)(i))]).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(j)).subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(gap_score)));
                java.math.BigInteger max_val_1 = java.math.BigInteger.valueOf(0);
                if (diag_1.compareTo(max_val_1) > 0) {
                    max_val_1 = new java.math.BigInteger(String.valueOf(diag_1));
                }
                if (delete_1.compareTo(max_val_1) > 0) {
                    max_val_1 = new java.math.BigInteger(String.valueOf(delete_1));
                }
                if (insert_1.compareTo(max_val_1) > 0) {
                    max_val_1 = new java.math.BigInteger(String.valueOf(insert_1));
                }
score_1[_idx((score_1).length, (long)(i))][(int)((long)(j))] = new java.math.BigInteger(String.valueOf(max_val_1));
            }
        }
        return ((java.math.BigInteger[][])(score_1));
    }

    static String traceback(java.math.BigInteger[][] score, String query, String subject, java.math.BigInteger match_score, java.math.BigInteger mismatch_score, java.math.BigInteger gap_score) {
        String q_1 = query.toUpperCase();
        String s_3 = subject.toUpperCase();
        java.math.BigInteger max_value_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_max_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger j_max_1 = java.math.BigInteger.valueOf(0);
        for (int i = 0; i < score.length; i++) {
            for (int j = 0; j < score[_idx((score).length, (long)(i))].length; j++) {
                if (score[_idx((score).length, (long)(i))][_idx((score[_idx((score).length, (long)(i))]).length, (long)(j))].compareTo(max_value_1) > 0) {
                    max_value_1 = new java.math.BigInteger(String.valueOf(score[_idx((score).length, (long)(i))][_idx((score[_idx((score).length, (long)(i))]).length, (long)(j))]));
                    i_max_1 = new java.math.BigInteger(String.valueOf(i));
                    j_max_1 = new java.math.BigInteger(String.valueOf(j));
                }
            }
        }
        java.math.BigInteger i_1 = new java.math.BigInteger(String.valueOf(i_max_1));
        java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(j_max_1));
        String align1_1 = "";
        String align2_1 = "";
        java.math.BigInteger gap_penalty_1 = new java.math.BigInteger(String.valueOf(score_function("-", "-", new java.math.BigInteger(String.valueOf(match_score)), new java.math.BigInteger(String.valueOf(mismatch_score)), new java.math.BigInteger(String.valueOf(gap_score)))));
        if (i_1.compareTo(java.math.BigInteger.valueOf(0)) == 0 || j_1.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return "";
        }
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) > 0 && j_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            String qc_3 = _substr(q_1, (int)(((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(i_1)).longValue()));
            String sc_3 = _substr(s_3, (int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)(((java.math.BigInteger)(j_1)).longValue()));
            if (score[_idx((score).length, ((java.math.BigInteger)(i_1)).longValue())][_idx((score[_idx((score).length, ((java.math.BigInteger)(i_1)).longValue())]).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(score[_idx((score).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((score[_idx((score).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(score_function(qc_3, sc_3, new java.math.BigInteger(String.valueOf(match_score)), new java.math.BigInteger(String.valueOf(mismatch_score)), new java.math.BigInteger(String.valueOf(gap_score))))) == 0) {
                align1_1 = qc_3 + align1_1;
                align2_1 = sc_3 + align2_1;
                i_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
            } else             if (score[_idx((score).length, ((java.math.BigInteger)(i_1)).longValue())][_idx((score[_idx((score).length, ((java.math.BigInteger)(i_1)).longValue())]).length, ((java.math.BigInteger)(j_1)).longValue())].compareTo(score[_idx((score).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((score[_idx((score).length, ((java.math.BigInteger)(i_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_1)).longValue())].add(gap_penalty_1)) == 0) {
                align1_1 = qc_3 + align1_1;
                align2_1 = "-" + align2_1;
                i_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
            } else {
                align1_1 = "-" + align1_1;
                align2_1 = sc_3 + align2_1;
                j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
            }
        }
        return align1_1 + "\n" + align2_1;
    }
    public static void main(String[] args) {
        score_2 = ((java.math.BigInteger[][])(smith_waterman(query, subject, java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate())))));
        System.out.println(traceback(((java.math.BigInteger[][])(score_2)), query, subject, java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(2)).negate()))));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
