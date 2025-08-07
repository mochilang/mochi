public class Main {

    static int[] max_tasks(int[][] tasks_info) {
        int[] order = ((int[])(new int[]{}));
        int i = 0;
        while (i < tasks_info.length) {
            order = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order), java.util.stream.IntStream.of(i)).toArray()));
            i = i + 1;
        }
        int n = order.length;
        i = 0;
        while (i < n) {
            int j = i + 1;
            while (j < n) {
                if (tasks_info[order[j]][1] > tasks_info[order[i]][1]) {
                    int tmp = order[i];
order[i] = order[j];
order[j] = tmp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        int[] result = ((int[])(new int[]{}));
        int pos = 1;
        i = 0;
        while (i < n) {
            int id = order[i];
            int deadline = tasks_info[id][0];
            if (deadline >= pos) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(id)).toArray()));
            }
            i = i + 1;
            pos = pos + 1;
        }
        return result;
    }

    static void main() {
        int[][] ex1 = ((int[][])(new int[][]{new int[]{4, 20}, new int[]{1, 10}, new int[]{1, 40}, new int[]{1, 30}}));
        int[][] ex2 = ((int[][])(new int[][]{new int[]{1, 10}, new int[]{2, 20}, new int[]{3, 30}, new int[]{2, 40}}));
        System.out.println(_p(max_tasks(((int[][])(ex1)))));
        System.out.println(_p(max_tasks(((int[][])(ex2)))));
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
