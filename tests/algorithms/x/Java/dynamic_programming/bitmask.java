public class Main {

    static int count_assignments(int person, int[][] task_performed, int[] used) {
        if (person == task_performed.length) {
            return 1;
        }
        int total = 0;
        int[] tasks = ((int[])(task_performed[person]));
        int i = 0;
        while (i < tasks.length) {
            int t = tasks[i];
            if (!(java.util.Arrays.stream(used).anyMatch((x) -> ((Number)(x)).intValue() == t))) {
                total = total + count_assignments(person + 1, ((int[][])(task_performed)), ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(used), java.util.stream.IntStream.of(t)).toArray())));
            }
            i = i + 1;
        }
        return total;
    }

    static int count_no_of_ways(int[][] task_performed) {
        return count_assignments(0, ((int[][])(task_performed)), ((int[])(new int[]{})));
    }

    static void main() {
        int[][] task_performed = ((int[][])(new int[][]{new int[]{1, 3, 4}, new int[]{1, 2, 5}, new int[]{3, 4}}));
        System.out.println(_p(count_no_of_ways(((int[][])(task_performed)))));
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
