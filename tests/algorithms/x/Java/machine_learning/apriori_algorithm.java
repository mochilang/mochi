public class Main {
    static class Itemset {
        String[] items;
        long support;
        Itemset(String[] items, long support) {
            this.items = items;
            this.support = support;
        }
        Itemset() {}
        @Override public String toString() {
            return String.format("{'items': %s, 'support': %s}", String.valueOf(items), String.valueOf(support));
        }
    }

    static Itemset[] frequent_itemsets = new Itemset[0];

    static String[][] load_data() {
        return new String[][]{new String[]{"milk"}, new String[]{"milk", "butter"}, new String[]{"milk", "bread"}, new String[]{"milk", "bread", "chips"}};
    }

    static boolean contains_string(String[] xs, String s) {
        for (String v : xs) {
            if ((v.equals(s))) {
                return true;
            }
        }
        return false;
    }

    static boolean is_subset(String[] candidate, String[] transaction) {
        for (String it : candidate) {
            if (!(Boolean)contains_string(((String[])(transaction)), it)) {
                return false;
            }
        }
        return true;
    }

    static boolean lists_equal(String[] a, String[] b) {
        if (a.length != b.length) {
            return false;
        }
        long i_1 = 0L;
        while (i_1 < a.length) {
            if (!(a[(int)((long)(i_1))].equals(b[(int)((long)(i_1))]))) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static boolean contains_list(String[][] itemset, String[] item) {
        for (String[] l : itemset) {
            if (((Boolean)(lists_equal(((String[])(l)), ((String[])(item)))))) {
                return true;
            }
        }
        return false;
    }

    static long count_list(String[][] itemset, String[] item) {
        long c = 0L;
        for (String[] l : itemset) {
            if (((Boolean)(lists_equal(((String[])(l)), ((String[])(item)))))) {
                c = c + 1;
            }
        }
        return c;
    }

    static String[][] slice_list(String[][] xs, long start) {
        String[][] res = ((String[][])(new String[][]{}));
        long i_3 = start;
        while (i_3 < xs.length) {
            res = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(xs[(int)((long)(i_3))])).toArray(String[][]::new)));
            i_3 = i_3 + 1;
        }
        return res;
    }

    static String[][][] combinations_lists(String[][] xs, long k) {
        String[][][] result = ((String[][][])(new String[][][]{}));
        if (k == 0) {
            result = ((String[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new String[][]{})).toArray(String[][][]::new)));
            return result;
        }
        long i_5 = 0L;
        while (i_5 < xs.length) {
            String[] head_1 = ((String[])(xs[(int)((long)(i_5))]));
            String[][] tail_1 = ((String[][])(slice_list(((String[][])(xs)), i_5 + 1)));
            String[][][] tail_combos_1 = ((String[][][])(combinations_lists(((String[][])(tail_1)), k - 1)));
            for (String[][] combo : tail_combos_1) {
                String[][] new_combo_1 = ((String[][])(new String[][]{}));
                new_combo_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_combo_1), java.util.stream.Stream.of(head_1)).toArray(String[][]::new)));
                for (String[] c : combo) {
                    new_combo_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_combo_1), java.util.stream.Stream.of(c)).toArray(String[][]::new)));
                }
                result = ((String[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new_combo_1)).toArray(String[][][]::new)));
            }
            i_5 = i_5 + 1;
        }
        return result;
    }

    static String[][][] prune(String[][] itemset, String[][][] candidates, long length) {
        String[][][] pruned = ((String[][][])(new String[][][]{}));
        for (String[][] candidate : candidates) {
            boolean is_subsequence_1 = true;
            for (String[] item : candidate) {
                if (!(Boolean)contains_list(((String[][])(itemset)), ((String[])(item))) || count_list(((String[][])(itemset)), ((String[])(item))) < length - 1) {
                    is_subsequence_1 = false;
                    break;
                }
            }
            if (is_subsequence_1) {
                pruned = ((String[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(pruned), java.util.stream.Stream.of(candidate)).toArray(String[][][]::new)));
            }
        }
        return pruned;
    }

    static String[] sort_strings(String[] xs) {
        String[] res_1 = ((String[])(new String[]{}));
        for (String s : xs) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(s)).toArray(String[]::new)));
        }
        long i_7 = 0L;
        while (i_7 < res_1.length) {
            long j_1 = i_7 + 1;
            while (j_1 < res_1.length) {
                if ((res_1[(int)((long)(j_1))].compareTo(res_1[(int)((long)(i_7))]) < 0)) {
                    String tmp_1 = res_1[(int)((long)(i_7))];
res_1[(int)((long)(i_7))] = res_1[(int)((long)(j_1))];
res_1[(int)((long)(j_1))] = tmp_1;
                }
                j_1 = j_1 + 1;
            }
            i_7 = i_7 + 1;
        }
        return res_1;
    }

    static String itemset_to_string(String[] xs) {
        String s = "[";
        long i_9 = 0L;
        while (i_9 < xs.length) {
            if (i_9 > 0) {
                s = s + ", ";
            }
            s = s + "'" + xs[(int)((long)(i_9))] + "'";
            i_9 = i_9 + 1;
        }
        s = s + "]";
        return s;
    }

    static Itemset[] apriori(String[][] data, long min_support) {
        String[][] itemset = ((String[][])(new String[][]{}));
        for (String[] transaction : data) {
            String[] t_1 = ((String[])(new String[]{}));
            for (String v : transaction) {
                t_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(t_1), java.util.stream.Stream.of(v)).toArray(String[]::new)));
            }
            itemset = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(itemset), java.util.stream.Stream.of(t_1)).toArray(String[][]::new)));
        }
        Itemset[] frequent_1 = ((Itemset[])(new Itemset[]{}));
        long length_1 = 1L;
        while (itemset.length > 0) {
            long[] counts_1 = ((long[])(new long[]{}));
            long idx_1 = 0L;
            while (idx_1 < itemset.length) {
                counts_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(counts_1), java.util.stream.LongStream.of(0L)).toArray()));
                idx_1 = idx_1 + 1;
            }
            for (String[] transaction : data) {
                long j_3 = 0L;
                while (j_3 < itemset.length) {
                    Object candidate_1 = itemset[(int)((long)(j_3))];
                    if (((Boolean)(is_subset(((String[])(candidate_1)), ((String[])(transaction)))))) {
counts_1[(int)((long)(j_3))] = counts_1[(int)((long)(j_3))] + 1;
                    }
                    j_3 = j_3 + 1;
                }
            }
            String[][] new_itemset_1 = ((String[][])(new String[][]{}));
            long k_1 = 0L;
            while (k_1 < itemset.length) {
                if (counts_1[(int)((long)(k_1))] >= min_support) {
                    new_itemset_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_itemset_1), java.util.stream.Stream.of(itemset[(int)((long)(k_1))])).toArray(String[][]::new)));
                }
                k_1 = k_1 + 1;
            }
            itemset = new_itemset_1;
            long m_1 = 0L;
            while (m_1 < itemset.length) {
                String[] sorted_item_1 = ((String[])(sort_strings(((String[])(itemset[(int)((long)(m_1))])))));
                frequent_1 = ((Itemset[])(java.util.stream.Stream.concat(java.util.Arrays.stream(frequent_1), java.util.stream.Stream.of(new Itemset(sorted_item_1, counts_1[(int)((long)(m_1))]))).toArray(Itemset[]::new)));
                m_1 = m_1 + 1;
            }
            length_1 = length_1 + 1;
            String[][][] combos_1 = ((String[][][])(combinations_lists(((String[][])(itemset)), length_1)));
            itemset = prune(((String[][])(itemset)), ((String[][][])(combos_1)), length_1);
        }
        return frequent_1;
    }
    public static void main(String[] args) {
        frequent_itemsets = ((Itemset[])(apriori(((String[][])(load_data())), 2L)));
        for (Itemset fi : frequent_itemsets) {
            System.out.println(String.valueOf(itemset_to_string(((String[])(fi.items)))) + ": " + _p(fi.support));
        }
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
