public class Main {

    static int solution() {
        int[][] triangle = ((int[][])(new int[][]{new int[]{75}, new int[]{95, 64}, new int[]{17, 47, 82}, new int[]{18, 35, 87, 10}, new int[]{20, 4, 82, 47, 65}, new int[]{19, 1, 23, 75, 3, 34}, new int[]{88, 2, 77, 73, 7, 63, 67}, new int[]{99, 65, 4, 28, 6, 16, 70, 92}, new int[]{41, 41, 26, 56, 83, 40, 80, 70, 33}, new int[]{41, 48, 72, 33, 47, 32, 37, 16, 94, 29}, new int[]{53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14}, new int[]{70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57}, new int[]{91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48}, new int[]{63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31}, new int[]{4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23}}));
        int i = 1;
        while (i < triangle.length) {
            int j = 0;
            while (j < triangle[i].length) {
                int[] prev_row = ((int[])(triangle[i - 1]));
                int number1 = j != prev_row.length ? prev_row[j] : 0;
                int number2 = j > 0 ? prev_row[j - 1] : 0;
                int max_val = number1 > number2 ? number1 : number2;
triangle[i][j] = triangle[i][j] + max_val;
                j = j + 1;
            }
            i = i + 1;
        }
        int[] last = ((int[])(triangle[triangle.length - 1]));
        int k = 0;
        int best = 0;
        while (k < last.length) {
            if (last[k] > best) {
                best = last[k];
            }
            k = k + 1;
        }
        return best;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution()));
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
