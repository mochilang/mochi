public class Main {

    static int[] iterator_values(int[][] matrix) {
        int[] result = ((int[])(new int[]{}));
        for (int[] row : matrix) {
            for (int value : row) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(value)).toArray()));
            }
        }
        return result;
    }

    static int index_2d_array_in_1d(int[][] array, int index) {
        int rows = array.length;
        int cols = array[0].length;
        if (rows == 0 || cols == 0) {
            throw new RuntimeException(String.valueOf("no items in array"));
        }
        if (index < 0 || index >= rows * cols) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        return array[((Number)(index / cols)).intValue()][Math.floorMod(index, cols)];
    }
    public static void main(String[] args) {
        System.out.println(_p(iterator_values(((int[][])(new int[][]{new int[]{5}, new int[]{-523}, new int[]{-1}, new int[]{34}, new int[]{0}})))));
        System.out.println(_p(iterator_values(((int[][])(new int[][]{new int[]{5, -523, -1}, new int[]{34, 0}})))));
        System.out.println(_p(index_2d_array_in_1d(((int[][])(new int[][]{new int[]{0, 1, 2, 3}, new int[]{4, 5, 6, 7}, new int[]{8, 9, 10, 11}})), 5)));
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
