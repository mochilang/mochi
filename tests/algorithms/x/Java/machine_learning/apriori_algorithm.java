public class Main {
    static class Itemset {
        String[] items;
        int support;
        Itemset(String[] items, int support) {
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
        int i = 0;
        while (i < a.length) {
            if (!(a[i].equals(b[i]))) {
                return false;
            }
            i = i + 1;
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

    static int count_list(String[][] itemset, String[] item) {
        int c = 0;
        for (String[] l : itemset) {
            if (((Boolean)(lists_equal(((String[])(l)), ((String[])(item)))))) {
                c = c + 1;
            }
        }
        return c;
    }

    static String[][] slice_list(String[][] xs, int start) {
        String[][] res = ((String[][])(new String[][]{}));
        int i_1 = start;
        while (i_1 < xs.length) {
            res = ((String[][])(appendObj(res, xs[i_1])));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String[][][] combinations_lists(String[][] xs, int k) {
        String[][][] result = ((String[][][])(new String[][][]{}));
        if (k == 0) {
            result = ((String[][][])(appendObj(result, new String[][]{})));
            return result;
        }
        int i_2 = 0;
        while (i_2 < xs.length) {
            String[] head = ((String[])(xs[i_2]));
            String[][] tail = ((String[][])(slice_list(((String[][])(xs)), i_2 + 1)));
            String[][][] tail_combos = ((String[][][])(combinations_lists(((String[][])(tail)), k - 1)));
            for (String[][] combo : tail_combos) {
                String[][] new_combo = ((String[][])(new String[][]{}));
                new_combo = ((String[][])(appendObj(new_combo, head)));
                for (String[] c : combo) {
                    new_combo = ((String[][])(appendObj(new_combo, c)));
                }
                result = ((String[][][])(appendObj(result, new_combo)));
            }
            i_2 = i_2 + 1;
        }
        return result;
    }

    static String[][][] prune(String[][] itemset, String[][][] candidates, int length) {
        String[][][] pruned = ((String[][][])(new String[][][]{}));
        for (String[][] candidate : candidates) {
            boolean is_subsequence = true;
            for (String[] item : candidate) {
                if (!(Boolean)contains_list(((String[][])(itemset)), ((String[])(item))) || count_list(((String[][])(itemset)), ((String[])(item))) < length - 1) {
                    is_subsequence = false;
                    break;
                }
            }
            if (is_subsequence) {
                pruned = ((String[][][])(appendObj(pruned, candidate)));
            }
        }
        return pruned;
    }

    static String[] sort_strings(String[] xs) {
        String[] res_1 = ((String[])(new String[]{}));
        for (String s : xs) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(s)).toArray(String[]::new)));
        }
        int i_3 = 0;
        while (i_3 < res_1.length) {
            int j = i_3 + 1;
            while (j < res_1.length) {
                if ((res_1[j].compareTo(res_1[i_3]) < 0)) {
                    String tmp = res_1[i_3];
res_1[i_3] = res_1[j];
res_1[j] = tmp;
                }
                j = j + 1;
            }
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static String itemset_to_string(String[] xs) {
        String s = "[";
        int i_4 = 0;
        while (i_4 < xs.length) {
            if (i_4 > 0) {
                s = s + ", ";
            }
            s = s + "'" + xs[i_4] + "'";
            i_4 = i_4 + 1;
        }
        s = s + "]";
        return s;
    }

    static Itemset[] apriori(String[][] data, int min_support) {
        Object itemset = new String[][]{};
        for (String[] transaction : data) {
            String[] t = ((String[])(new String[]{}));
            for (String v : transaction) {
                t = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(t), java.util.stream.Stream.of(v)).toArray(String[]::new)));
            }
            itemset = ((String[][])(appendObj(itemset, t)));
        }
        Itemset[] frequent = ((Itemset[])(new Itemset[]{}));
        int length = 1;
        while (itemset.length > 0) {
            int[] counts = ((int[])(new int[]{}));
            int idx = 0;
            while (idx < itemset.length) {
                counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(0)).toArray()));
                idx = idx + 1;
            }
            for (String[] transaction : data) {
                int j_1 = 0;
                while (j_1 < itemset.length) {
                    String[] candidate = ((String[])(itemset[j_1]));
                    if (((Boolean)(is_subset(((String[])(candidate)), ((String[])(transaction)))))) {
counts[j_1] = counts[j_1] + 1;
                    }
                    j_1 = j_1 + 1;
                }
            }
            String[][] new_itemset = ((String[][])(new String[][]{}));
            int k = 0;
            while (k < itemset.length) {
                if (counts[k] >= min_support) {
                    new_itemset = ((String[][])(appendObj(new_itemset, itemset[k])));
                }
                k = k + 1;
            }
            itemset = ((String[][])(new_itemset));
            int m = 0;
            while (m < itemset.length) {
                String[] sorted_item = ((String[])(sort_strings(((String[])(itemset[m])))));
                frequent = ((Itemset[])(java.util.stream.Stream.concat(java.util.Arrays.stream(frequent), java.util.stream.Stream.of(new Itemset(sorted_item, counts[m]))).toArray(Itemset[]::new)));
                m = m + 1;
            }
            length = length + 1;
            String[][][] combos = ((String[][][])(combinations_lists(((String[][])(itemset)), length)));
            itemset = prune(((String[][])(itemset)), ((String[][][])(combos)), length);
        }
        return frequent;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            frequent_itemsets = ((Itemset[])(apriori(((String[][])(load_data())), 2)));
            for (Itemset fi : frequent_itemsets) {
                System.out.println(String.valueOf(itemset_to_string(((String[])(fi.items)))) + ": " + _p(fi.support));
            }
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
