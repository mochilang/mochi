public class Main {
    static class LinkedList {
        int[] data;
        LinkedList(int[] data) {
            this.data = data;
        }
        LinkedList() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }


    static LinkedList empty_list() {
        return new LinkedList(new int[]{});
    }

    static LinkedList append_value(LinkedList list, int value) {
        int[] d = ((int[])(list.data));
        d = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(d), java.util.stream.IntStream.of(value)).toArray()));
        return new LinkedList(d);
    }

    static LinkedList extend_list(LinkedList list, int[] items) {
        LinkedList result = list;
        int i = 0;
        while (i < items.length) {
            result = append_value(result, items[i]);
            i = i + 1;
        }
        return result;
    }

    static String to_string(LinkedList list) {
        if (list.data.length == 0) {
            return "";
        }
        String s = _p(_geti(list.data, 0));
        int i_1 = 1;
        while (i_1 < list.data.length) {
            s = s + " -> " + _p(_geti(list.data, i_1));
            i_1 = i_1 + 1;
        }
        return s;
    }

    static LinkedList make_linked_list(int[] items) {
        if (items.length == 0) {
            throw new RuntimeException(String.valueOf("The Elements List is empty"));
        }
        LinkedList ll = empty_list();
        ll = extend_list(ll, ((int[])(items)));
        return ll;
    }

    static String in_reverse(LinkedList list) {
        if (list.data.length == 0) {
            return "";
        }
        int i_2 = list.data.length - 1;
        String s_1 = _p(_geti(list.data, i_2));
        i_2 = i_2 - 1;
        while (i_2 >= 0) {
            s_1 = s_1 + " <- " + _p(_geti(list.data, i_2));
            i_2 = i_2 - 1;
        }
        return s_1;
    }

    static void main() {
        LinkedList linked_list = make_linked_list(((int[])(new int[]{14, 52, 14, 12, 43})));
        System.out.println("Linked List:  " + String.valueOf(to_string(linked_list)));
        System.out.println("Reverse List: " + String.valueOf(in_reverse(linked_list)));
    }
    public static void main(String[] args) {
        main();
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
