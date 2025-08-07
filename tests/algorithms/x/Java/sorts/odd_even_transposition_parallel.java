public class Main {

    static int[] odd_even_transposition(int[] xs) {
        int[] arr = ((int[])(xs));
        int n = arr.length;
        int phase = 0;
        while (phase < n) {
            int start = Math.floorMod(phase, 2) == 0 ? 0 : 1;
            int i = start;
            while (i + 1 < n) {
                if (arr[i] > arr[i + 1]) {
                    int tmp = arr[i];
arr[i] = arr[i + 1];
arr[i + 1] = tmp;
                }
                i = i + 2;
            }
            phase = phase + 1;
        }
        return arr;
    }

    static void main() {
        int[] data = ((int[])(new int[]{10, 9, 8, 7, 6, 5, 4, 3, 2, 1}));
        System.out.println("Initial List");
        System.out.println(_p(data));
        int[] sorted = ((int[])(odd_even_transposition(((int[])(data)))));
        System.out.println("Sorted List");
        System.out.println(_p(sorted));
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
