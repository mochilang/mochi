public class Main {

    static int[] cycle_sort(int[] arr) {
        int n = arr.length;
        int cycle_start = 0;
        while (cycle_start < n - 1) {
            int item = arr[cycle_start];
            int pos = cycle_start;
            int i = cycle_start + 1;
            while (i < n) {
                if (arr[i] < item) {
                    pos = pos + 1;
                }
                i = i + 1;
            }
            if (pos == cycle_start) {
                cycle_start = cycle_start + 1;
                continue;
            }
            while (item == arr[pos]) {
                pos = pos + 1;
            }
            int temp = arr[pos];
arr[pos] = item;
            item = temp;
            while (pos != cycle_start) {
                pos = cycle_start;
                i = cycle_start + 1;
                while (i < n) {
                    if (arr[i] < item) {
                        pos = pos + 1;
                    }
                    i = i + 1;
                }
                while (item == arr[pos]) {
                    pos = pos + 1;
                }
                int temp2 = arr[pos];
arr[pos] = item;
                item = temp2;
            }
            cycle_start = cycle_start + 1;
        }
        return arr;
    }
    public static void main(String[] args) {
        System.out.println(_p(cycle_sort(((int[])(new int[]{4, 3, 2, 1})))));
        System.out.println(_p(cycle_sort(((int[])(new int[]{-4, 20, 0, -50, 100, -1})))));
        System.out.println(_p(cycle_sort(((int[])(new int[]{})))));
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
