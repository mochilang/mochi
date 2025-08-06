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

    static LinkedList push(LinkedList list, int value) {
        int[] res = ((int[])(new int[]{value}));
        res = ((int[])(concat(res, list.data)));
        return new LinkedList(res);
    }

    static LinkedList swap_nodes(LinkedList list, int v1, int v2) {
        if (v1 == v2) {
            return list;
        }
        int idx1 = 0 - 1;
        int idx2 = 0 - 1;
        int i = 0;
        while (i < list.data.length) {
            if (list.data[i] == v1 && idx1 == 0 - 1) {
                idx1 = i;
            }
            if (list.data[i] == v2 && idx2 == 0 - 1) {
                idx2 = i;
            }
            i = i + 1;
        }
        if (idx1 == 0 - 1 || idx2 == 0 - 1) {
            return list;
        }
        int[] res_1 = ((int[])(list.data));
        int temp = res_1[idx1];
res_1[idx1] = res_1[idx2];
res_1[idx2] = temp;
        return new LinkedList(res_1);
    }

    static String to_string(LinkedList list) {
        return _p(list.data);
    }

    static void main() {
        LinkedList ll = empty_list();
        int i_1 = 5;
        while (i_1 > 0) {
            ll = push(ll, i_1);
            i_1 = i_1 - 1;
        }
        System.out.println("Original Linked List: " + String.valueOf(to_string(ll)));
        ll = swap_nodes(ll, 1, 4);
        System.out.println("Modified Linked List: " + String.valueOf(to_string(ll)));
        System.out.println("After swapping the nodes whose data is 1 and 4.");
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
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
