public class Main {

    static int bitwise_and(int a, int b) {
        int result = 0;
        int bit = 1;
        int x = a;
        int y = b;
        while (x > 0 || y > 0) {
            int abit = Math.floorMod(x, 2);
            int bbit = Math.floorMod(y, 2);
            if (abit == 1 && bbit == 1) {
                result = result + bit;
            }
            x = Math.floorDiv(x, 2);
            y = Math.floorDiv(y, 2);
            bit = bit * 2;
        }
        return result;
    }

    static int[] list_of_submasks(int mask) {
        if (mask <= 0) {
            throw new RuntimeException(String.valueOf("mask needs to be positive integer, your input " + _p(mask)));
        }
        int[] all_submasks = ((int[])(new int[]{}));
        int submask = mask;
        while (submask != 0) {
            all_submasks = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(all_submasks), java.util.stream.IntStream.of(submask)).toArray()));
            submask = bitwise_and(submask - 1, mask);
        }
        return all_submasks;
    }
    public static void main(String[] args) {
        System.out.println(_p(list_of_submasks(15)));
        System.out.println(_p(list_of_submasks(13)));
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
