public class Main {

    static boolean contains(int[] xs, int value) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == value) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static int solution(int n) {
        int[] zmulti = ((int[])(new int[]{}));
        int[] xmulti = ((int[])(new int[]{}));
        int temp = 1;
        while (true) {
            int result = 3 * temp;
            if (result < n) {
                zmulti = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(zmulti), java.util.stream.IntStream.of(result)).toArray()));
                temp = temp + 1;
            } else {
                break;
            }
        }
        temp = 1;
        while (true) {
            int result_1 = 5 * temp;
            if (result_1 < n) {
                xmulti = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(xmulti), java.util.stream.IntStream.of(result_1)).toArray()));
                temp = temp + 1;
            } else {
                break;
            }
        }
        int[] collection = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < zmulti.length) {
            int v = zmulti[i_1];
            if (!(Boolean)contains(((int[])(collection)), v)) {
                collection = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(collection), java.util.stream.IntStream.of(v)).toArray()));
            }
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < xmulti.length) {
            int v_1 = xmulti[i_1];
            if (!(Boolean)contains(((int[])(collection)), v_1)) {
                collection = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(collection), java.util.stream.IntStream.of(v_1)).toArray()));
            }
            i_1 = i_1 + 1;
        }
        int total = 0;
        i_1 = 0;
        while (i_1 < collection.length) {
            total = total + collection[i_1];
            i_1 = i_1 + 1;
        }
        return total;
    }

    static void test_solution() {
        if (solution(3) != 0) {
            throw new RuntimeException(String.valueOf("solution(3) failed"));
        }
        if (solution(4) != 3) {
            throw new RuntimeException(String.valueOf("solution(4) failed"));
        }
        if (solution(10) != 23) {
            throw new RuntimeException(String.valueOf("solution(10) failed"));
        }
        if (solution(600) != 83700) {
            throw new RuntimeException(String.valueOf("solution(600) failed"));
        }
    }

    static void main() {
        test_solution();
        System.out.println("solution() = " + _p(solution(1000)));
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
