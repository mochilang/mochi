public class Main {

    static int[] lexical_order(int max_number) {
        int[] result = ((int[])(new int[]{}));
        int[] stack = ((int[])(((int[])(new int[]{1}))));
        while (stack.length > 0) {
            int idx = stack.length - 1;
            int num = stack[idx];
            stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, idx)));
            if (num > max_number) {
                continue;
            }
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(num)).toArray()));
            if (Math.floorMod(num, 10) != 9) {
                stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(num + 1)).toArray()));
            }
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(num * 10)).toArray()));
        }
        return result;
    }

    static String join_ints(int[] xs) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + " ";
            }
            res = res + _p(_geti(xs, i));
            i = i + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        System.out.println(join_ints(((int[])(lexical_order(13)))));
        System.out.println(_p(lexical_order(1)));
        System.out.println(join_ints(((int[])(lexical_order(20)))));
        System.out.println(join_ints(((int[])(lexical_order(25)))));
        System.out.println(_p(lexical_order(12)));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
