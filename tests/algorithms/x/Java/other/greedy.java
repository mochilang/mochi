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

    static String[] food;
    static double[] value;
    static double[] weight;
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
        return t.value / t.weight;
    }

    static Thing[] build_menu(String[] names, double[] values, double[] weights) {
        Thing[] menu = ((Thing[])(new Thing[]{}));
        int i = 0;
        while (i < values.length && i < names.length && i < weights.length) {
            menu = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(menu), java.util.stream.Stream.of(new Thing(names[i], values[i], weights[i]))).toArray(Thing[]::new)));
            i = i + 1;
        }
        return menu;
    }

    static Thing[] sort_desc(Thing[] items, java.util.function.Function<Thing,Double> key_func) {
        Thing[] arr = ((Thing[])(new Thing[]{}));
        int i_1 = 0;
        while (i_1 < items.length) {
            arr = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(items[i_1])).toArray(Thing[]::new)));
            i_1 = i_1 + 1;
        }
        int j = 1;
        while (j < arr.length) {
            Thing key_item = arr[j];
            double key_val = key_func.apply(key_item);
            int k = j - 1;
            while (k >= 0 && key_func.apply(arr[k]) < key_val) {
arr[k + 1] = arr[k];
                k = k - 1;
            }
arr[k + 1] = key_item;
            j = j + 1;
        }
        return arr;
    }

    static GreedyResult greedy(Thing[] items, double max_cost, java.util.function.Function<Thing,Double> key_func) {
        Thing[] items_copy = ((Thing[])(sort_desc(((Thing[])(items)), key_func)));
        Thing[] result = ((Thing[])(new Thing[]{}));
        double total_value = 0.0;
        double total_cost = 0.0;
        int i_2 = 0;
        while (i_2 < items_copy.length) {
            Thing it = items_copy[i_2];
            double w = get_weight(it);
            if (total_cost + w <= max_cost) {
                result = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(it)).toArray(Thing[]::new)));
                total_cost = total_cost + w;
                total_value = total_value + get_value(it);
            }
            i_2 = i_2 + 1;
        }
        return new GreedyResult(result, total_value);
    }

    static String thing_to_string(Thing t) {
        return "Thing(" + t.name + ", " + _p(t.value) + ", " + _p(t.weight) + ")";
    }

    static String list_to_string(Thing[] ts) {
        String s = "[";
        int i_3 = 0;
        while (i_3 < ts.length) {
            s = s + String.valueOf(thing_to_string(ts[i_3]));
            if (i_3 < ts.length - 1) {
                s = s + ", ";
            }
            i_3 = i_3 + 1;
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            food = ((String[])(new String[]{"Burger", "Pizza", "Coca Cola", "Rice", "Sambhar", "Chicken", "Fries", "Milk"}));
            value = ((double[])(new double[]{80.0, 100.0, 60.0, 70.0, 50.0, 110.0, 90.0, 60.0}));
            weight = ((double[])(new double[]{40.0, 60.0, 40.0, 70.0, 100.0, 85.0, 55.0, 70.0}));
            foods = ((Thing[])(build_menu(((String[])(food)), ((double[])(value)), ((double[])(weight)))));
            System.out.println(list_to_string(((Thing[])(foods))));
            res = greedy(((Thing[])(foods)), 500.0, Main::get_value);
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
        return String.valueOf(v);
    }
}
