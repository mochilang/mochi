public class Main {

    static Object make_node(String name, long count, Object parent) {
        return ((Object)(new java.util.LinkedHashMap<String, Object>() {{ put("name", name); put("count", count); put("parent", parent); put("children", new java.util.LinkedHashMap<String, Object>()); put("node_link", null); }}));
    }

    static void update_header(Object node_to_test, Object target_node) {
        Object current = node_to_test;
        while (!(((Object)(((java.util.Map)current)).get("node_link")) == null)) {
            current = (Object)(((Object)(((java.util.Map)current)).get("node_link")));
        }
current.node_link = target_node;
    }

    static void update_tree(String[] items, Object in_tree, Object header_table, long count) {
        String first = items[(int)(0L)];
        Object children_1 = (Object)(((Object)(((java.util.Map)in_tree)).get("children")));
        if (children_1.containsKey(first)) {
            Object child_1 = (Object)(((Object)(((java.util.Map)children_1)).get(first)));
child_1.count = ((Number)(((Object)(((java.util.Map)child_1)).get("count")))).intValue() + (long)(count);
children_1[(int)((long)(first))] = child_1;
in_tree.children = children_1;
        } else {
            Object new_node_1 = make_node(first, (long)(count), in_tree);
children_1[(int)((long)(first))] = new_node_1;
in_tree.children = children_1;
            Object entry_1 = (Object)(((Object)(((java.util.Map)header_table)).get(first)));
            if ((((Object)(((java.util.Map)entry_1)).get("node")) == null)) {
entry_1.node = new_node_1;
            } else {
                update_header((Object)(((Object)(((java.util.Map)entry_1)).get("node"))), new_node_1);
            }
header_table[(int)((long)(first))] = entry_1;
        }
        if ((long)(items.length) > 1L) {
            String[] rest_1 = ((String[])(java.util.Arrays.copyOfRange(items, (int)(1L), (int)((long)(items.length)))));
            update_tree(((String[])(rest_1)), (Object)(((Object)(((java.util.Map)children_1)).get(first))), header_table, (long)(count));
        }
    }

    static String[] sort_items(String[] items, Object header_table) {
        String[] arr = ((String[])(items));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(arr.length)) {
            long j_1 = (long)((long)(i_1) + 1L);
            while ((long)(j_1) < (long)(arr.length)) {
                if (String.valueOf(((Object)(((java.util.Map)((Object)(((java.util.Map)header_table)).get(arr[(int)((long)(i_1))])))).get("count"))).compareTo(String.valueOf(((Object)(((java.util.Map)((Object)(((java.util.Map)header_table)).get(arr[(int)((long)(j_1))])))).get("count")))) < 0) {
                    String tmp_1 = arr[(int)((long)(i_1))];
arr[(int)((long)(i_1))] = arr[(int)((long)(j_1))];
arr[(int)((long)(j_1))] = tmp_1;
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return arr;
    }

    static Object create_tree(String[][] data_set, long min_sup) {
        java.util.Map<Object,Object> counts = ((java.util.Map<Object,Object>)(new java.util.LinkedHashMap<Object, Object>()));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(data_set.length)) {
            String[] trans_1 = ((String[])(data_set[(int)((long)(i_3))]));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(trans_1.length)) {
                String item_1 = trans_1[(int)((long)(j_3))];
                if (counts.containsKey(item_1)) {
counts.put(item_1, ((Number)(((Object)(counts).get(item_1)))).intValue() + 1L);
                } else {
counts.put(item_1, 1);
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        java.util.Map<Object,Object> header_table_1 = ((java.util.Map<Object,Object>)(new java.util.LinkedHashMap<Object, Object>()));
        for (Object k : counts.keySet()) {
            Object cnt_1 = (Object)(((Object)(counts).get(k)));
            if (((Number)(cnt_1)).intValue() >= (long)(min_sup)) {
header_table_1.put(k, new java.util.LinkedHashMap<String, Object>() {{ put("count", cnt_1); put("node", null); }});
            }
        }
        Object[] freq_items_1 = new double[]{};
        for (Object k : header_table_1.keySet()) {
            freq_items_1 = ((Object[])(java.util.stream.Stream.concat(java.util.Arrays.stream(freq_items_1), java.util.stream.Stream.of(k)).toArray(Object[]::new)));
        }
        if ((long)(freq_items_1.length) == 0L) {
            return ((Object)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("tree", (Object)(make_node("Null Set", 1L, null))), java.util.Map.entry("header", (Object)(new java.util.LinkedHashMap<String, Object>()))))));
        }
        Object fp_tree_1 = make_node("Null Set", 1L, null);
        i_3 = 0L;
        while ((long)(i_3) < (long)(data_set.length)) {
            String[] tran_1 = ((String[])(data_set[(int)((long)(i_3))]));
            String[] local_items_1 = ((String[])(new String[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(tran_1.length)) {
                String item_3 = tran_1[(int)((long)(j_5))];
                if (header_table_1.containsKey(item_3)) {
                    local_items_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(local_items_1), java.util.stream.Stream.of(item_3)).toArray(String[]::new)));
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            if ((long)(local_items_1.length) > 0L) {
                local_items_1 = ((String[])(sort_items(((String[])(local_items_1)), header_table_1)));
                update_tree(((String[])(local_items_1)), fp_tree_1, header_table_1, 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return ((Object)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("tree", (Object)(fp_tree_1)), java.util.Map.entry("header", (Object)(header_table_1))))));
    }

    static String[] ascend_tree(Object leaf_node, String[] path) {
        String[] prefix = ((String[])(path));
        if (!(((Object)(((java.util.Map)leaf_node)).get("parent")) == null)) {
            prefix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(prefix), java.util.stream.Stream.of(((Object)(((java.util.Map)leaf_node)).get("name")))).toArray(String[]::new)));
            prefix = ((String[])(ascend_tree((Object)(((Object)(((java.util.Map)leaf_node)).get("parent"))), ((String[])(prefix)))));
        } else {
            prefix = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(prefix), java.util.stream.Stream.of(((Object)(((java.util.Map)leaf_node)).get("name")))).toArray(String[]::new)));
        }
        return prefix;
    }

    static Object[] find_prefix_path(String base_pat, Object tree_node) {
        java.util.Map<String, String[]>[] cond_pats = ((java.util.Map<String, String[]>[])((java.util.Map<String, String[]>[])new java.util.Map[]{}));
        Object node_1 = tree_node;
        while (!(node_1 == null)) {
            String[] prefix_2 = ((String[])(ascend_tree(node_1, ((String[])(new String[]{})))));
            if ((long)(prefix_2.length) > 1L) {
                String[] items_1 = ((String[])(java.util.Arrays.copyOfRange(prefix_2, (int)(1L), (int)((long)(prefix_2.length)))));
                cond_pats = ((java.util.Map<String, String[]>[])(appendObj((java.util.Map<String, String[]>[])cond_pats, new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry("items", ((String[])(items_1))), java.util.Map.entry("count", (String[])(((Object)(((java.util.Map)node_1)).get("count")))))))));
            }
            node_1 = (Object)(((Object)(((java.util.Map)node_1)).get("node_link")));
        }
        return _toObjectArray(cond_pats);
    }

    static String[][] mine_tree(Object in_tree, Object header_table, long min_sup, String[] pre_fix, String[][] freq_item_list) {
        String[][] freq_list = ((String[][])(freq_item_list));
        Object[] items_3 = new double[]{};
        for (var k : header_table) {
            items_3 = ((Object[])(java.util.stream.Stream.concat(java.util.Arrays.stream(items_3), java.util.stream.Stream.of(k)).toArray(Object[]::new)));
        }
        Object[] sorted_items_1 = ((Object[])(items_3));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(sorted_items_1.length)) {
            long j_7 = (long)((long)(i_5) + 1L);
            while ((long)(j_7) < (long)(sorted_items_1.length)) {
                if (String.valueOf(((Object)(((java.util.Map)header_table[sorted_items_1[(int)((long)(i_5))]])).get("count"))).compareTo(String.valueOf(((Object)(((java.util.Map)header_table[sorted_items_1[(int)((long)(j_7))]])).get("count")))) > 0) {
                    Object tmp_3 = sorted_items_1[(int)((long)(i_5))];
sorted_items_1[(int)((long)(i_5))] = sorted_items_1[(int)((long)(j_7))];
sorted_items_1[(int)((long)(j_7))] = tmp_3;
                }
                j_7 = (long)((long)(j_7) + 1L);
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(sorted_items_1.length)) {
            Object base_pat_1 = sorted_items_1[(int)((long)(idx_1))];
            String[] new_freq_1 = ((String[])(pre_fix));
            new_freq_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_freq_1), java.util.stream.Stream.of(base_pat_1)).toArray(String[]::new)));
            freq_list = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(freq_list), java.util.stream.Stream.of(new String[][]{new_freq_1})).toArray(String[][]::new)));
            Object[] cond_pats_2 = ((Object[])(find_prefix_path((String)(base_pat_1), (Object)(((Object)(((java.util.Map)header_table[base_pat_1])).get("node"))))));
            Object[] cond_dataset_1 = new double[]{};
            long p_1 = 0L;
            while ((long)(p_1) < (long)(cond_pats_2.length)) {
                Object pat_1 = cond_pats_2[(int)((long)(p_1))];
                long r_1 = 0L;
                while ((long)(r_1) < ((Number)(((Object)(((java.util.Map)pat_1)).get("count")))).intValue()) {
                    cond_dataset_1 = ((Object[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cond_dataset_1), java.util.stream.Stream.of(((Object)(((java.util.Map)pat_1)).get("items")))).toArray(Object[]::new)));
                    r_1 = (long)((long)(r_1) + 1L);
                }
                p_1 = (long)((long)(p_1) + 1L);
            }
            Object res2_1 = create_tree(((String[][])(cond_dataset_1)), (long)(min_sup));
            Object my_tree_1 = (Object)(((Object)(((java.util.Map)res2_1)).get("tree")));
            Object my_head_1 = (Object)(((Object)(((java.util.Map)res2_1)).get("header")));
            if ((long)(String.valueOf(my_head_1).length()) > 0L) {
                freq_list = ((String[][])(mine_tree(my_tree_1, my_head_1, (long)(min_sup), ((String[])(new_freq_1)), ((String[][])(freq_list)))));
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
        return freq_list;
    }

    static String list_to_string(String[] xs) {
        String s = "[";
        long i_7 = 0L;
        while ((long)(i_7) < (long)(xs.length)) {
            s = s + xs[(int)((long)(i_7))];
            if ((long)(i_7) < (long)((long)(xs.length) - 1L)) {
                s = s + ", ";
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return s + "]";
    }

    static void main() {
        String[][] data_set = ((String[][])(new String[][]{new String[]{"bread", "milk", "cheese"}, new String[]{"bread", "milk"}, new String[]{"bread", "diapers"}, new String[]{"bread", "milk", "diapers"}, new String[]{"milk", "diapers"}, new String[]{"milk", "cheese"}, new String[]{"diapers", "cheese"}, new String[]{"bread", "milk", "cheese", "diapers"}}));
        Object res_1 = create_tree(((String[][])(data_set)), 3L);
        Object fp_tree_3 = (Object)(((Object)(((java.util.Map)res_1)).get("tree")));
        Object header_table_3 = (Object)(((Object)(((java.util.Map)res_1)).get("header")));
        String[][] freq_items_3 = ((String[][])(new String[][]{}));
        freq_items_3 = ((String[][])(mine_tree(fp_tree_3, header_table_3, 3L, ((String[])(new String[]{})), ((String[][])(freq_items_3)))));
        System.out.println(data_set.length);
        System.out.println(String.valueOf(header_table_3).length());
        long i_9 = 0L;
        while ((long)(i_9) < (long)(freq_items_3.length)) {
            System.out.println(list_to_string(((String[])(freq_items_3[(int)((long)(i_9))]))));
            i_9 = (long)((long)(i_9) + 1L);
        }
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static Object[] _toObjectArray(Object v) {
        if (v instanceof Object[]) return (Object[]) v;
        if (v instanceof int[]) return java.util.Arrays.stream((int[]) v).boxed().toArray();
        if (v instanceof double[]) return java.util.Arrays.stream((double[]) v).boxed().toArray();
        if (v instanceof long[]) return java.util.Arrays.stream((long[]) v).boxed().toArray();
        if (v instanceof boolean[]) { boolean[] a = (boolean[]) v; Object[] out = new Object[a.length]; for (int i = 0; i < a.length; i++) out[i] = a[i]; return out; }
        return (Object[]) v;
    }
}
