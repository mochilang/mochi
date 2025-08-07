public class Main {

    static int solution(int number) {
        int[] partitions = ((int[])(new int[]{1}));
        int i = partitions.length;
        while (true) {
            int item = 0;
            int j = 1;
            while (true) {
                int sign = Math.floorMod(j, 2) == 0 ? -1 : 1;
                int index = Math.floorDiv((j * j * 3 - j), 2);
                if (index > i) {
                    break;
                }
                item = item + partitions[i - index] * sign;
                item = Math.floorMod(item, number);
                index = index + j;
                if (index > i) {
                    break;
                }
                item = item + partitions[i - index] * sign;
                item = Math.floorMod(item, number);
                j = j + 1;
            }
            if (item == 0) {
                return i;
            }
            partitions = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(partitions), java.util.stream.IntStream.of(item)).toArray()));
            i = i + 1;
        }
        return 0;
    }

    static void main() {
        System.out.println(_p(solution(1)));
        System.out.println(_p(solution(9)));
        System.out.println(_p(solution(1000000)));
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
