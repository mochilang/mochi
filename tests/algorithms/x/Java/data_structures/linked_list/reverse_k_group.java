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


    static String to_string(LinkedList list) {
        if (list.data.length == 0) {
            return "";
        }
        String s = _p(_geti(list.data, 0));
        int i = 1;
        while (i < list.data.length) {
            s = s + " -> " + _p(_geti(list.data, i));
            i = i + 1;
        }
        return s;
    }

    static LinkedList reverse_k_nodes(LinkedList list, int k) {
        if (k <= 1) {
            return list;
        }
        int[] res = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < list.data.length) {
            int j = 0;
            int[] group = ((int[])(new int[]{}));
            while (j < k && i_1 + j < list.data.length) {
                group = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(group), java.util.stream.IntStream.of(list.data[i_1 + j])).toArray()));
                j = j + 1;
            }
            if (group.length == k) {
                int g = k - 1;
                while (g >= 0) {
                    res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(group[g])).toArray()));
                    g = g - 1;
                }
            } else {
                int g_1 = 0;
                while (g_1 < group.length) {
                    res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(group[g_1])).toArray()));
                    g_1 = g_1 + 1;
                }
            }
            i_1 = i_1 + k;
        }
        return new LinkedList(res);
    }

    static void main() {
        LinkedList ll = new LinkedList(new int[]{1, 2, 3, 4, 5});
        System.out.println("Original Linked List: " + String.valueOf(to_string(ll)));
        int k = 2;
        ll = reverse_k_nodes(ll, k);
        System.out.println("After reversing groups of size " + _p(k) + ": " + String.valueOf(to_string(ll)));
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
