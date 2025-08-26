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
    static double[] value = ((double[])(new double[]{(double)(80.0), (double)(100.0), (double)(60.0), (double)(70.0), (double)(50.0), (double)(110.0), (double)(90.0), (double)(60.0)}));
    static double[] weight = ((double[])(new double[]{(double)(40.0), (double)(60.0), (double)(40.0), (double)(70.0), (double)(100.0), (double)(85.0), (double)(55.0), (double)(70.0)}));
    static Thing[] foods;
    static GreedyResult res;

    static double get_value(Thing t) {
        return (double)(t.value);
    }

    static double get_weight(Thing t) {
        return (double)(t.weight);
    }

    static String get_name(Thing t) {
        return t.name;
    }

    static double value_weight(Thing t) {
        return (double)((double)(t.value) / (double)(t.weight));
    }

    static Thing[] build_menu(String[] names, double[] values, double[] weights) {
        Thing[] menu = ((Thing[])(new Thing[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(values.length))) < 0 && i_1.compareTo(new java.math.BigInteger(String.valueOf(names.length))) < 0 && i_1.compareTo(new java.math.BigInteger(String.valueOf(weights.length))) < 0) {
            menu = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(menu), java.util.stream.Stream.of(new Thing(names[_idx((names).length, ((java.math.BigInteger)(i_1)).longValue())], (double)(values[_idx((values).length, ((java.math.BigInteger)(i_1)).longValue())]), (double)(weights[_idx((weights).length, ((java.math.BigInteger)(i_1)).longValue())])))).toArray(Thing[]::new)));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((Thing[])(menu));
    }

    static Thing[] sort_desc(Thing[] items, java.util.function.Function<Thing,Double> key_func) {
        Thing[] arr = ((Thing[])(new Thing[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(items.length))) < 0) {
            arr = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(items[_idx((items).length, ((java.math.BigInteger)(i_3)).longValue())])).toArray(Thing[]::new)));
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(1);
        while (j_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            Thing key_item_1 = arr[_idx((arr).length, ((java.math.BigInteger)(j_1)).longValue())];
            double key_val_1 = (double)(key_func.apply(key_item_1));
            java.math.BigInteger k_1 = j_1.subtract(java.math.BigInteger.valueOf(1));
            while (k_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0 && (double)(key_func.apply(arr[_idx((arr).length, ((java.math.BigInteger)(k_1)).longValue())])) < (double)(key_val_1)) {
arr[(int)(((java.math.BigInteger)(k_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = arr[_idx((arr).length, ((java.math.BigInteger)(k_1)).longValue())];
                k_1 = k_1.subtract(java.math.BigInteger.valueOf(1));
            }
arr[(int)(((java.math.BigInteger)(k_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = key_item_1;
            j_1 = j_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((Thing[])(arr));
    }

    static GreedyResult greedy(Thing[] items, double max_cost, java.util.function.Function<Thing,Double> key_func) {
        Thing[] items_copy = ((Thing[])(sort_desc(((Thing[])(items)), key_func)));
        Thing[] result_1 = ((Thing[])(new Thing[]{}));
        double total_value_1 = (double)(0.0);
        double total_cost_1 = (double)(0.0);
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(items_copy.length))) < 0) {
            Thing it_1 = items_copy[_idx((items_copy).length, ((java.math.BigInteger)(i_5)).longValue())];
            double w_1 = (double)(get_weight(it_1));
            if ((double)((double)(total_cost_1) + (double)(w_1)) <= (double)(max_cost)) {
                result_1 = ((Thing[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(it_1)).toArray(Thing[]::new)));
                total_cost_1 = (double)((double)(total_cost_1) + (double)(w_1));
                total_value_1 = (double)((double)(total_value_1) + (double)(get_value(it_1)));
            }
            i_5 = i_5.add(java.math.BigInteger.valueOf(1));
        }
        return new GreedyResult(((Thing[])(result_1)), (double)(total_value_1));
    }

    static String thing_to_string(Thing t) {
        return "Thing(" + t.name + ", " + _p(t.value) + ", " + _p(t.weight) + ")";
    }

    static String list_to_string(Thing[] ts) {
        String s = "[";
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(ts.length))) < 0) {
            s = s + String.valueOf(thing_to_string(ts[_idx((ts).length, ((java.math.BigInteger)(i_7)).longValue())]));
            if (i_7.compareTo(new java.math.BigInteger(String.valueOf(ts.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                s = s + ", ";
            }
            i_7 = i_7.add(java.math.BigInteger.valueOf(1));
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
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
