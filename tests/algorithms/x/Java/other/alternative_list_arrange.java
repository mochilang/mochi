public class Main {
    interface Item {}

    static class Int implements Item {
        long value;
        Int(long value) {
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

    static Item from_int(long x) {
        return ((Item)(new Int(x)));
    }

    static Item from_string(String s) {
        return ((Item)(new Str(s)));
    }

    static String item_to_string(Item it) {
        return it instanceof Int ? _p(((Int)(it)).value) : ((Str)(it)).value;
    }

    static Item[] alternative_list_arrange(Item[] first, Item[] second) {
        long len1 = (long)(first.length);
        long len2_1 = (long)(second.length);
        long abs_len_1 = (long)((long)(len1) > (long)(len2_1) ? len1 : len2_1);
        Item[] result_1 = ((Item[])(new Item[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(abs_len_1)) {
            if ((long)(i_1) < (long)(len1)) {
                result_1 = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(first[(int)((long)(i_1))])).toArray(Item[]::new)));
            }
            if ((long)(i_1) < (long)(len2_1)) {
                result_1 = ((Item[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(second[(int)((long)(i_1))])).toArray(Item[]::new)));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result_1;
    }

    static String list_to_string(Item[] xs) {
        String s = "[";
        long i_3 = 0L;
        while ((long)(i_3) < (long)(xs.length)) {
            s = s + String.valueOf(item_to_string(xs[(int)((long)(i_3))]));
            if ((long)(i_3) < (long)((long)(xs.length) - 1L)) {
                s = s + ", ";
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        example1 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_int(1L), from_int(2L), from_int(3L), from_int(4L), from_int(5L)})), ((Item[])(new Item[]{from_string("A"), from_string("B"), from_string("C")})))));
        System.out.println(list_to_string(((Item[])(example1))));
        example2 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_string("A"), from_string("B"), from_string("C")})), ((Item[])(new Item[]{from_int(1L), from_int(2L), from_int(3L), from_int(4L), from_int(5L)})))));
        System.out.println(list_to_string(((Item[])(example2))));
        example3 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_string("X"), from_string("Y"), from_string("Z")})), ((Item[])(new Item[]{from_int(9L), from_int(8L), from_int(7L), from_int(6L)})))));
        System.out.println(list_to_string(((Item[])(example3))));
        example4 = ((Item[])(alternative_list_arrange(((Item[])(new Item[]{from_int(1L), from_int(2L), from_int(3L), from_int(4L), from_int(5L)})), ((Item[])(new Item[]{})))));
        System.out.println(list_to_string(((Item[])(example4))));
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
