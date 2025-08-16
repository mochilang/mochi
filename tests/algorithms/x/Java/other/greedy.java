public class Main {
    static class Thing {
        String name;
        double value;
        double weight;
        Thing(String name, double value, double weight) {
            this.name = name;
            this.value = value;
            this.weight = weight;
        }
        Thing() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'value': %s, 'weight': %s}", String.valueOf(name), String.valueOf(value), String.valueOf(weight));
        }
    }

    static class GreedyResult {
        Thing[] items;
        double total_value;
        GreedyResult(Thing[] items, double total_value) {
            this.items = items;
            this.total_value = total_value;
        }
        GreedyResult() {}
        @Override public String toString() {
            return String.format("{'items': %s, 'total_value': %s}", String.valueOf(items), String.valueOf(total_value));
        }
    }

    static String[] food = ((String[])(new String[]{"Burger", "Pizza", "Coca Cola", "Rice", "Sambhar", "Chicken", "Fries", "Milk"}));
    static double[] value = ((double[])(new double[]{80.0, 100.0, 60.0, 70.0, 50.0, 110.0, 90.0, 60.0}));
    static double[] weight = ((double[])(new double[]{40.0, 60.0, 40.0, 70.0, 100.0, 85.0, 55.0, 70.0}));
    static Thing[] foods;
    static GreedyResult res;

    static double get_value(Thing t) {
        return t.value;
    }

    static double get_weight(Thing t) {
        return t.weight;
    }

    static String get_name(Thing t) {
        return t.name;
    }

    static double value_weight(Thing t) {
        return (double)(t.value) / (double)(t.weight);
    }

    static Thing[] build_menu(String[] names, double[] values, double[] weights) {
        Thing[] menu = ((Thing[])(new Thing[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(values.length) && (long)(i_1) < (long)(names.length) && (long)(i_1) < (long)(weights.length)) {
            menu = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(menu), java.util.stream.Stream.of(new Thing(names[(int)((long)(i_1))], values[(int)((long)(i_1))], weights[(int)((long)(i_1))]))).toArray(Thing[]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return menu;
    }

    static Thing[] sort_desc(Thing[] items, java.util.function.Function<Thing,Double> key_func) {
        Thing[] arr = ((Thing[])(new Thing[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(items.length)) {
            arr = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(items[(int)((long)(i_3))])).toArray(Thing[]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long j_1 = 1L;
        while ((long)(j_1) < (long)(arr.length)) {
            Thing key_item_1 = arr[(int)((long)(j_1))];
            double key_val_1 = (double)(key_func.apply(key_item_1));
            long k_1 = (long)((long)(j_1) - 1L);
            while ((long)(k_1) >= 0L && (double)(key_func.apply(arr[(int)((long)(k_1))])) < (double)(key_val_1)) {
arr[(int)((long)((long)(k_1) + 1L))] = arr[(int)((long)(k_1))];
                k_1 = (long)((long)(k_1) - 1L);
            }
arr[(int)((long)((long)(k_1) + 1L))] = key_item_1;
            j_1 = (long)((long)(j_1) + 1L);
        }
        return arr;
    }

    static GreedyResult greedy(Thing[] items, double max_cost, java.util.function.Function<Thing,Double> key_func) {
        Thing[] items_copy = ((Thing[])(sort_desc(((Thing[])(items)), key_func)));
        Thing[] result_1 = ((Thing[])(new Thing[]{}));
        double total_value_1 = (double)(0.0);
        double total_cost_1 = (double)(0.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(items_copy.length)) {
            Thing it_1 = items_copy[(int)((long)(i_5))];
            double w_1 = (double)(get_weight(it_1));
            if ((double)((double)(total_cost_1) + (double)(w_1)) <= (double)(max_cost)) {
                result_1 = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(it_1)).toArray(Thing[]::new)));
                total_cost_1 = (double)((double)(total_cost_1) + (double)(w_1));
                total_value_1 = (double)((double)(total_value_1) + (double)(get_value(it_1)));
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        return new GreedyResult(result_1, total_value_1);
    }

    static String thing_to_string(Thing t) {
        return "Thing(" + t.name + ", " + _p(t.value) + ", " + _p(t.weight) + ")";
    }

    static String list_to_string(Thing[] ts) {
        String s = "[";
        long i_7 = 0L;
        while ((long)(i_7) < (long)(ts.length)) {
            s = s + String.valueOf(thing_to_string(ts[(int)((long)(i_7))]));
            if ((long)(i_7) < (long)((long)(ts.length) - 1L)) {
                s = s + ", ";
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            foods = ((Thing[])(build_menu(((String[])(food)), ((double[])(value)), ((double[])(weight)))));
            System.out.println(list_to_string(((Thing[])(foods))));
            res = greedy(((Thing[])(foods)), (double)(500.0), Main::get_value);
            System.out.println(list_to_string(((Thing[])(res.items))));
            System.out.println(_p(res.total_value));
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
