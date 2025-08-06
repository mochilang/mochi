public class Main {

    static int largest_rectangle_area(int[] heights) {
        int[] stack = ((int[])(new int[]{}));
        int max_area = 0;
        int[] hs = ((int[])(heights));
        hs = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(hs), java.util.stream.IntStream.of(0)).toArray()));
        int i = 0;
        while (i < hs.length) {
            while (stack.length > 0 && hs[i] < hs[stack[stack.length - 1]]) {
                int top = stack[stack.length - 1];
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
                int height = hs[top];
                int width = i;
                if (stack.length > 0) {
                    width = i - stack[stack.length - 1] - 1;
                }
                int area = height * width;
                if (area > max_area) {
                    max_area = area;
                }
            }
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(i)).toArray()));
            i = i + 1;
        }
        return max_area;
    }
    public static void main(String[] args) {
        System.out.println(_p(largest_rectangle_area(((int[])(new int[]{2, 1, 5, 6, 2, 3})))));
        System.out.println(_p(largest_rectangle_area(((int[])(new int[]{2, 4})))));
        System.out.println(_p(largest_rectangle_area(((int[])(new int[]{6, 2, 5, 4, 5, 1, 6})))));
        System.out.println(_p(largest_rectangle_area(((int[])(new int[]{1})))));
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
