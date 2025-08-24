public class Main {
    static class Heap {
        java.math.BigInteger[][] arr;
        java.util.Map<java.math.BigInteger,java.math.BigInteger> pos_map;
        java.math.BigInteger size;
        java.util.function.Function<java.math.BigInteger,java.math.BigInteger> key;
        Heap(java.math.BigInteger[][] arr, java.util.Map<java.math.BigInteger,java.math.BigInteger> pos_map, java.math.BigInteger size, java.util.function.Function<java.math.BigInteger,java.math.BigInteger> key) {
            this.arr = arr;
            this.pos_map = pos_map;
            this.size = size;
            this.key = key;
        }
        Heap() {}
        @Override public String toString() {
            return String.format("{'arr': %s, 'pos_map': %s, 'size': %s, 'key': %s}", String.valueOf(arr), String.valueOf(pos_map), String.valueOf(size), String.valueOf(key));
        }
    }

    static Heap h = null;

    static Heap new_heap(java.util.function.Function<java.math.BigInteger,java.math.BigInteger> key) {
        return new Heap(((java.math.BigInteger[][])(new java.math.BigInteger[][]{})), ((java.util.Map<java.math.BigInteger,java.math.BigInteger>)(new java.util.LinkedHashMap<java.math.BigInteger, java.math.BigInteger>())), java.math.BigInteger.valueOf(0), key);
    }

    static java.math.BigInteger parent(java.math.BigInteger i) {
        if (i.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            return new java.math.BigInteger(String.valueOf((i.subtract(java.math.BigInteger.valueOf(1))).divide(java.math.BigInteger.valueOf(2))));
        }
        return new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
    }

    static java.math.BigInteger left(java.math.BigInteger i, java.math.BigInteger size) {
        java.math.BigInteger l = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(2).multiply(i).add(java.math.BigInteger.valueOf(1))));
        if (l.compareTo(size) < 0) {
            return new java.math.BigInteger(String.valueOf(l));
        }
        return new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
    }

    static java.math.BigInteger right(java.math.BigInteger i, java.math.BigInteger size) {
        java.math.BigInteger r = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(2).multiply(i).add(java.math.BigInteger.valueOf(2))));
        if (r.compareTo(size) < 0) {
            return new java.math.BigInteger(String.valueOf(r));
        }
        return new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
    }

    static void swap(Heap h, java.math.BigInteger i, java.math.BigInteger j) {
        java.math.BigInteger[][] arr = ((java.math.BigInteger[][])(h.arr));
        java.math.BigInteger item_i_1 = new java.math.BigInteger(String.valueOf(arr[_idx((arr).length, ((java.math.BigInteger)(i)).longValue())][_idx((arr[_idx((arr).length, ((java.math.BigInteger)(i)).longValue())]).length, 0L)]));
        java.math.BigInteger item_j_1 = new java.math.BigInteger(String.valueOf(arr[_idx((arr).length, ((java.math.BigInteger)(j)).longValue())][_idx((arr[_idx((arr).length, ((java.math.BigInteger)(j)).longValue())]).length, 0L)]));
        java.util.Map<java.math.BigInteger,java.math.BigInteger> pm_1 = h.pos_map;
pm_1.put(item_i_1, new java.math.BigInteger(String.valueOf(j.add(java.math.BigInteger.valueOf(1)))));
pm_1.put(item_j_1, new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1)))));
h.pos_map = pm_1;
        java.math.BigInteger[] tmp_1 = ((java.math.BigInteger[])(arr[_idx((arr).length, ((java.math.BigInteger)(i)).longValue())]));
arr[(int)(((java.math.BigInteger)(i)).longValue())] = ((java.math.BigInteger[])(arr[_idx((arr).length, ((java.math.BigInteger)(j)).longValue())]));
arr[(int)(((java.math.BigInteger)(j)).longValue())] = ((java.math.BigInteger[])(tmp_1));
h.arr = arr;
    }

    static boolean cmp(Heap h, java.math.BigInteger i, java.math.BigInteger j) {
        java.math.BigInteger[][] arr_1 = ((java.math.BigInteger[][])(h.arr));
        return arr_1[_idx((arr_1).length, ((java.math.BigInteger)(i)).longValue())][_idx((arr_1[_idx((arr_1).length, ((java.math.BigInteger)(i)).longValue())]).length, 1L)].compareTo(arr_1[_idx((arr_1).length, ((java.math.BigInteger)(j)).longValue())][_idx((arr_1[_idx((arr_1).length, ((java.math.BigInteger)(j)).longValue())]).length, 1L)]) < 0;
    }

    static java.math.BigInteger get_valid_parent(Heap h, java.math.BigInteger i) {
        java.math.BigInteger vp = new java.math.BigInteger(String.valueOf(i));
        java.math.BigInteger l_2 = new java.math.BigInteger(String.valueOf(left(new java.math.BigInteger(String.valueOf(i)), new java.math.BigInteger(String.valueOf(h.size)))));
        if (l_2.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0 && (cmp(h, new java.math.BigInteger(String.valueOf(l_2)), new java.math.BigInteger(String.valueOf(vp))) == false)) {
            vp = new java.math.BigInteger(String.valueOf(l_2));
        }
        java.math.BigInteger r_2 = new java.math.BigInteger(String.valueOf(right(new java.math.BigInteger(String.valueOf(i)), new java.math.BigInteger(String.valueOf(h.size)))));
        if (r_2.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0 && (cmp(h, new java.math.BigInteger(String.valueOf(r_2)), new java.math.BigInteger(String.valueOf(vp))) == false)) {
            vp = new java.math.BigInteger(String.valueOf(r_2));
        }
        return new java.math.BigInteger(String.valueOf(vp));
    }

    static void heapify_up(Heap h, java.math.BigInteger index) {
        java.math.BigInteger idx = new java.math.BigInteger(String.valueOf(index));
        java.math.BigInteger p_1 = new java.math.BigInteger(String.valueOf(parent(new java.math.BigInteger(String.valueOf(idx)))));
        while (p_1.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0 && (cmp(h, new java.math.BigInteger(String.valueOf(idx)), new java.math.BigInteger(String.valueOf(p_1))) == false)) {
            swap(h, new java.math.BigInteger(String.valueOf(idx)), new java.math.BigInteger(String.valueOf(p_1)));
            idx = new java.math.BigInteger(String.valueOf(p_1));
            p_1 = new java.math.BigInteger(String.valueOf(parent(new java.math.BigInteger(String.valueOf(p_1)))));
        }
    }

    static void heapify_down(Heap h, java.math.BigInteger index) {
        java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(index));
        java.math.BigInteger vp_2 = new java.math.BigInteger(String.valueOf(get_valid_parent(h, new java.math.BigInteger(String.valueOf(idx_1)))));
        while (vp_2.compareTo(idx_1) != 0) {
            swap(h, new java.math.BigInteger(String.valueOf(idx_1)), new java.math.BigInteger(String.valueOf(vp_2)));
            idx_1 = new java.math.BigInteger(String.valueOf(vp_2));
            vp_2 = new java.math.BigInteger(String.valueOf(get_valid_parent(h, new java.math.BigInteger(String.valueOf(idx_1)))));
        }
    }

    static void update_item(Heap h, java.math.BigInteger item, java.math.BigInteger item_value) {
        java.util.Map<java.math.BigInteger,java.math.BigInteger> pm_2 = h.pos_map;
        if (((java.math.BigInteger)(pm_2).get(item)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return;
        }
        java.math.BigInteger index_1 = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(pm_2).get(item)).subtract(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger[][] arr_3 = ((java.math.BigInteger[][])(h.arr));
arr_3[(int)(((java.math.BigInteger)(index_1)).longValue())] = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(item)), new java.math.BigInteger(String.valueOf(h.key.apply(new java.math.BigInteger(String.valueOf(item_value)))))}));
h.arr = arr_3;
h.pos_map = pm_2;
        heapify_up(h, new java.math.BigInteger(String.valueOf(index_1)));
        heapify_down(h, new java.math.BigInteger(String.valueOf(index_1)));
    }

    static void delete_item(Heap h, java.math.BigInteger item) {
        java.util.Map<java.math.BigInteger,java.math.BigInteger> pm_3 = h.pos_map;
        if (((java.math.BigInteger)(pm_3).get(item)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return;
        }
        java.math.BigInteger index_3 = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(pm_3).get(item)).subtract(java.math.BigInteger.valueOf(1))));
pm_3.put(item, java.math.BigInteger.valueOf(0));
        java.math.BigInteger[][] arr_5 = ((java.math.BigInteger[][])(h.arr));
        java.math.BigInteger last_index_1 = new java.math.BigInteger(String.valueOf(h.size.subtract(java.math.BigInteger.valueOf(1))));
        if (index_3.compareTo(last_index_1) != 0) {
arr_5[(int)(((java.math.BigInteger)(index_3)).longValue())] = ((java.math.BigInteger[])(arr_5[_idx((arr_5).length, ((java.math.BigInteger)(last_index_1)).longValue())]));
            java.math.BigInteger moved_1 = new java.math.BigInteger(String.valueOf(arr_5[_idx((arr_5).length, ((java.math.BigInteger)(index_3)).longValue())][_idx((arr_5[_idx((arr_5).length, ((java.math.BigInteger)(index_3)).longValue())]).length, 0L)]));
pm_3.put(moved_1, new java.math.BigInteger(String.valueOf(index_3.add(java.math.BigInteger.valueOf(1)))));
        }
h.size = h.size.subtract(java.math.BigInteger.valueOf(1));
h.arr = arr_5;
h.pos_map = pm_3;
        if (h.size.compareTo(index_3) > 0) {
            heapify_up(h, new java.math.BigInteger(String.valueOf(index_3)));
            heapify_down(h, new java.math.BigInteger(String.valueOf(index_3)));
        }
    }

    static void insert_item(Heap h, java.math.BigInteger item, java.math.BigInteger item_value) {
        java.math.BigInteger[][] arr_6 = ((java.math.BigInteger[][])(h.arr));
        java.math.BigInteger arr_len_1 = new java.math.BigInteger(String.valueOf(arr_6.length));
        if (arr_len_1.compareTo(h.size) == 0) {
            arr_6 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_6), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(item)), new java.math.BigInteger(String.valueOf(h.key.apply(new java.math.BigInteger(String.valueOf(item_value)))))}))})).toArray(java.math.BigInteger[][]::new)));
        } else {
arr_6[(int)(((java.math.BigInteger)(h.size)).longValue())] = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(item)), new java.math.BigInteger(String.valueOf(h.key.apply(new java.math.BigInteger(String.valueOf(item_value)))))}));
        }
        java.util.Map<java.math.BigInteger,java.math.BigInteger> pm_5 = h.pos_map;
pm_5.put(item, new java.math.BigInteger(String.valueOf(h.size.add(java.math.BigInteger.valueOf(1)))));
h.size = h.size.add(java.math.BigInteger.valueOf(1));
h.arr = arr_6;
h.pos_map = pm_5;
        heapify_up(h, new java.math.BigInteger(String.valueOf(h.size.subtract(java.math.BigInteger.valueOf(1)))));
    }

    static java.math.BigInteger[] get_top(Heap h) {
        java.math.BigInteger[][] arr_7 = ((java.math.BigInteger[][])(h.arr));
        if (h.size.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            return ((java.math.BigInteger[])(arr_7[_idx((arr_7).length, 0L)]));
        }
        return ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
    }

    static java.math.BigInteger[] extract_top(Heap h) {
        java.math.BigInteger[] top = ((java.math.BigInteger[])(get_top(h)));
        if (new java.math.BigInteger(String.valueOf(top.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            delete_item(h, new java.math.BigInteger(String.valueOf(top[_idx((top).length, 0L)])));
        }
        return ((java.math.BigInteger[])(top));
    }

    static java.math.BigInteger identity(java.math.BigInteger x) {
        return new java.math.BigInteger(String.valueOf(x));
    }

    static java.math.BigInteger negate(java.math.BigInteger x) {
        return new java.math.BigInteger(String.valueOf((x).negate()));
    }
    public static void main(String[] args) {
        h = new_heap(Main::identity);
        insert_item(h, java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(34));
        insert_item(h, java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(31));
        insert_item(h, java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(37));
        System.out.println(_p(get_top(h)));
        System.out.println(_p(extract_top(h)));
        System.out.println(_p(extract_top(h)));
        System.out.println(_p(extract_top(h)));
        h = new_heap(Main::negate);
        insert_item(h, java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(34));
        insert_item(h, java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(31));
        insert_item(h, java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(37));
        System.out.println(_p(get_top(h)));
        System.out.println(_p(extract_top(h)));
        System.out.println(_p(extract_top(h)));
        System.out.println(_p(extract_top(h)));
        insert_item(h, java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(45));
        insert_item(h, java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(40));
        insert_item(h, java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(50));
        System.out.println(_p(get_top(h)));
        update_item(h, java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(30));
        System.out.println(_p(get_top(h)));
        delete_item(h, java.math.BigInteger.valueOf(10));
        System.out.println(_p(get_top(h)));
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
