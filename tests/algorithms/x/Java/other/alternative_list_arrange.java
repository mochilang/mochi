public class Main {
    interface Item {}

    static class Int implements Item {
        int value;
        Int(int value) {
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

    static Item from_int(int x) {
        return new Int(x);
    }

    static Item from_string(String s) {
        return new Str(s);
    }

    static String item_to_string(Item it) {
        return it instanceof Int ? _p(((Int)(it)).value) : ((Str)(it)).value;
    }

    static Item[] alternative_list_arrange(Item[] first, Item[] second) {
        int len1 = first.length;
        int len2 = second.length;
        int abs_len = len1 > len2 ? len1 : len2;
        Item[] result = ((Item[])(new Item[]{}));
        int i = 0;
        while (i < abs_len) {
            if (i < len1) {
                result = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(first[i])).toArray(Item[]::new)));
            }
            if (i < len2) {
                result = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(second[i])).toArray(Item[]::new)));
            }
            i = i + 1;
        }
        return result;
    }

    static String list_to_string(Item[] xs) {
        String s = "[";
        int i_1 = 0;
        while (i_1 < xs.length) {
            s = s + String.valueOf(item_to_string(xs[i_1]));
            if (i_1 < xs.length - 1) {
                s = s + ", ";
            }
            i_1 = i_1 + 1;
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            example1 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)})), ((Item[])(new Item[]{from_string("A"), from_string("B"), from_string("C")})))));
            System.out.println(list_to_string(((Item[])(example1))));
            example2 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_string("A"), from_string("B"), from_string("C")})), ((Item[])(new Item[]{from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)})))));
            System.out.println(list_to_string(((Item[])(example2))));
            example3 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_string("X"), from_string("Y"), from_string("Z")})), ((Item[])(new Item[]{from_int(9), from_int(8), from_int(7), from_int(6)})))));
            System.out.println(list_to_string(((Item[])(example3))));
            example4 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_int(1), from_int(2), from_int(3), from_int(4), from_int(5)})), ((Item[])(new Item[]{})))));
            System.out.println(list_to_string(((Item[])(example4))));
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
