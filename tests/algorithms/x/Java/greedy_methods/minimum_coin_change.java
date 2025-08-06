public class Main {

    static int[] find_minimum_change(int[] denominations, int value) {
        if (value <= 0) {
            return new int[]{};
        }
        int total = value;
        int[] answer = ((int[])(new int[]{}));
        int i = denominations.length - 1;
        while (i >= 0) {
            int denom = denominations[i];
            while (total >= denom) {
                total = total - denom;
                answer = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(answer), java.util.stream.IntStream.of(denom)).toArray()));
            }
            i = i - 1;
        }
        return answer;
    }
    public static void main(String[] args) {
        System.out.println(_p(find_minimum_change(((int[])(new int[]{1, 2, 5, 10, 20, 50, 100, 500, 2000})), 987)));
        System.out.println(_p(find_minimum_change(((int[])(new int[]{1, 5, 100, 500, 1000})), 456)));
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
