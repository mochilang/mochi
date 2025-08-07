public class Main {

    static int[] run(int n) {
        int limit = 200000;
        java.util.Map<Integer,Integer> counts = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        int p = 2;
        while (p <= limit) {
            if (!(Boolean)(counts.containsKey(p))) {
                int m = p;
                while (m <= limit) {
                    if (((Boolean)(counts.containsKey(m)))) {
counts.put(m, (int)(((int)(counts).getOrDefault(m, 0))) + 1);
                    } else {
counts.put(m, 1);
                    }
                    m = m + p;
                }
            }
            p = p + 1;
        }
        int streak = 0;
        int num = 2;
        while (num <= limit) {
            int c = counts.containsKey(num) ? ((int)(counts).getOrDefault(num, 0)) : 0;
            if (c == n) {
                streak = streak + 1;
                if (streak == n) {
                    int[] result = ((int[])(new int[]{}));
                    int start = num - n + 1;
                    int j = 0;
                    while (j < n) {
                        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(start + j)).toArray()));
                        j = j + 1;
                    }
                    return result;
                }
            } else {
                streak = 0;
            }
            num = num + 1;
        }
        return new int[]{};
    }

    static int solution(int n) {
        int[] res = ((int[])(run(n)));
        return res[0];
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(4)));
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
