public class Main {

    static int[] make_list(int n, int value) {
        int[] result = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return result;
    }

    static int min_value(int[] arr) {
        int m = arr[0];
        int i_1 = 1;
        while (i_1 < arr.length) {
            if (arr[i_1] < m) {
                m = arr[i_1];
            }
            i_1 = i_1 + 1;
        }
        return m;
    }

    static int max_value(int[] arr) {
        int m_1 = arr[0];
        int i_2 = 1;
        while (i_2 < arr.length) {
            if (arr[i_2] > m_1) {
                m_1 = arr[i_2];
            }
            i_2 = i_2 + 1;
        }
        return m_1;
    }

    static int[] pigeon_sort(int[] array) {
        if (array.length == 0) {
            return array;
        }
        int mn = min_value(((int[])(array)));
        int mx = max_value(((int[])(array)));
        int holes_range = mx - mn + 1;
        int[] holes = ((int[])(make_list(holes_range, 0)));
        int[] holes_repeat = ((int[])(make_list(holes_range, 0)));
        int i_3 = 0;
        while (i_3 < array.length) {
            int index = array[i_3] - mn;
holes[index] = array[i_3];
holes_repeat[index] = holes_repeat[index] + 1;
            i_3 = i_3 + 1;
        }
        int array_index = 0;
        int h = 0;
        while (h < holes_range) {
            while (holes_repeat[h] > 0) {
array[array_index] = holes[h];
                array_index = array_index + 1;
holes_repeat[h] = holes_repeat[h] - 1;
            }
            h = h + 1;
        }
        return array;
    }
    public static void main(String[] args) {
        System.out.println(_p(pigeon_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
        System.out.println(_p(pigeon_sort(((int[])(new int[]{})))));
        System.out.println(_p(pigeon_sort(((int[])(new int[]{-2, -5, -45})))));
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
