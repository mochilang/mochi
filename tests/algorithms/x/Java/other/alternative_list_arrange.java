public class Main {
    interface Item {}

    static class Int implements Item {
        java.math.BigInteger value;
        Int(java.math.BigInteger value) {
            this.value = value;
        }
        Int() {}
        @Override public String toString() {
            return String.format("{'value': %s}", String.valueOf(value));
        }
    }

    static class Str implements Item {
        String value;
        Str(String value) {
            this.value = value;
        }
        Str() {}
        @Override public String toString() {
            return String.format("{'value': '%s'}", String.valueOf(value));
        }
    }

    static Item[] example1;
    static Item[] example2;
    static Item[] example3;
    static Item[] example4;

    static Item from_int(java.math.BigInteger x) {
        return ((Item)(new Int(x)));
    }

    static Item from_string(String s) {
        return ((Item)(new Str(s)));
    }

    static String item_to_string(Item it) {
        return String.valueOf(it instanceof Int ? _p(((Int)(it)).value) : ((Str)(it)).value);
    }

    static Item[] alternative_list_arrange(Item[] first, Item[] second) {
        java.math.BigInteger len1 = new java.math.BigInteger(String.valueOf(first.length));
        java.math.BigInteger len2_1 = new java.math.BigInteger(String.valueOf(second.length));
        java.math.BigInteger abs_len_1 = len1.compareTo(len2_1) > 0 ? len1 : len2_1;
        Item[] result_1 = ((Item[])(new Item[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(abs_len_1) < 0) {
            if (i_1.compareTo(len1) < 0) {
                result_1 = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(first[_idx((first).length, ((java.math.BigInteger)(i_1)).longValue())])).toArray(Item[]::new)));
            }
            if (i_1.compareTo(len2_1) < 0) {
                result_1 = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(second[_idx((second).length, ((java.math.BigInteger)(i_1)).longValue())])).toArray(Item[]::new)));
            }
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((Item[])(result_1));
    }

    static String list_to_string(Item[] xs) {
        String s = "[";
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            s = s + String.valueOf(item_to_string(xs[_idx((xs).length, ((java.math.BigInteger)(i_3)).longValue())]));
            if (i_3.compareTo(new java.math.BigInteger(String.valueOf(xs.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                s = s + ", ";
            }
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            example1 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_int(java.math.BigInteger.valueOf(1)), from_int(java.math.BigInteger.valueOf(2)), from_int(java.math.BigInteger.valueOf(3)), from_int(java.math.BigInteger.valueOf(4)), from_int(java.math.BigInteger.valueOf(5))})), ((Item[])(new Item[]{from_string("A"), from_string("B"), from_string("C")})))));
            System.out.println(list_to_string(((Item[])(example1))));
            example2 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_string("A"), from_string("B"), from_string("C")})), ((Item[])(new Item[]{from_int(java.math.BigInteger.valueOf(1)), from_int(java.math.BigInteger.valueOf(2)), from_int(java.math.BigInteger.valueOf(3)), from_int(java.math.BigInteger.valueOf(4)), from_int(java.math.BigInteger.valueOf(5))})))));
            System.out.println(list_to_string(((Item[])(example2))));
            example3 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_string("X"), from_string("Y"), from_string("Z")})), ((Item[])(new Item[]{from_int(java.math.BigInteger.valueOf(9)), from_int(java.math.BigInteger.valueOf(8)), from_int(java.math.BigInteger.valueOf(7)), from_int(java.math.BigInteger.valueOf(6))})))));
            System.out.println(list_to_string(((Item[])(example3))));
            example4 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_int(java.math.BigInteger.valueOf(1)), from_int(java.math.BigInteger.valueOf(2)), from_int(java.math.BigInteger.valueOf(3)), from_int(java.math.BigInteger.valueOf(4)), from_int(java.math.BigInteger.valueOf(5))})), ((Item[])(new Item[]{})))));
            System.out.println(list_to_string(((Item[])(example4))));
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
