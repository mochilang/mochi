public class Main {
    static int[] example;
    static int[] result;
    static String output = null;
    static int j = 0;

    static int[] pigeonhole_sort(int[] arr) {
        if (arr.length == 0) {
            return arr;
        }
        int min_val = ((Number)(_min(arr))).intValue();
        int max_val = ((Number)(_max(arr))).intValue();
        int size = max_val - min_val + 1;
        int[] holes = ((int[])(new int[]{}));
        int i = 0;
        while (i < size) {
            holes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(holes), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        i = 0;
        while (i < arr.length) {
            int x = arr[i];
            int index = x - min_val;
holes[index] = holes[index] + 1;
            i = i + 1;
        }
        int sorted_index = 0;
        int count = 0;
        while (count < size) {
            while (holes[count] > 0) {
arr[sorted_index] = count + min_val;
holes[count] = holes[count] - 1;
                sorted_index = sorted_index + 1;
            }
            count = count + 1;
        }
        return arr;
    }
    public static void main(String[] args) {
        example = ((int[])(new int[]{8, 3, 2, 7, 4, 6, 8}));
        result = ((int[])(pigeonhole_sort(((int[])(example)))));
        output = "Sorted order is:";
        j = 0;
        while (j < result.length) {
            output = output + " " + _p(_geti(result, j));
            j = j + 1;
        }
        System.out.println(output);
    }

    static int _min(int[] a) {
        int m = a[0];
        for (int i = 1; i < a.length; i++) if (a[i] < m) m = a[i];
        return m;
    }

    static int _max(int[] a) {
        int m = a[0];
        for (int i = 1; i < a.length; i++) if (a[i] > m) m = a[i];
        return m;
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
