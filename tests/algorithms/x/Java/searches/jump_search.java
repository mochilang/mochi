public class Main {

    static int int_sqrt(int n) {
        int x = 0;
        while ((x + 1) * (x + 1) <= n) {
            x = x + 1;
        }
        return x;
    }

    static int jump_search(int[] arr, int item) {
        int arr_size = arr.length;
        int block_size = int_sqrt(arr_size);
        int prev = 0;
        int step = block_size;
        while (step < arr_size && arr[step - 1] < item) {
            prev = step;
            step = step + block_size;
            if (prev >= arr_size) {
                return -1;
            }
        }
        while (prev < arr_size && arr[prev] < item) {
            prev = prev + 1;
            if (prev == step) {
                return -1;
            }
        }
        if (prev < arr_size && arr[prev] == item) {
            return prev;
        }
        return -1;
    }

    static void main() {
        System.out.println(_p(jump_search(((int[])(new int[]{0, 1, 2, 3, 4, 5})), 3)));
        System.out.println(_p(jump_search(((int[])(new int[]{-5, -2, -1})), -1)));
        System.out.println(_p(jump_search(((int[])(new int[]{0, 5, 10, 20})), 8)));
        System.out.println(_p(jump_search(((int[])(new int[]{0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610})), 55)));
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
}
