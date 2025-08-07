public class Main {

    static int bisect_left(int[][] stacks, int value) {
        int low = 0;
        int high = stacks.length;
        while (low < high) {
            int mid = Math.floorDiv((low + high), 2);
            int[] stack = ((int[])(stacks[mid]));
            int top_idx = stack.length - 1;
            int top = stack[top_idx];
            if (top < value) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return low;
    }

    static int[] reverse_list(int[] src) {
        int[] res = ((int[])(new int[]{}));
        int i = src.length - 1;
        while (i >= 0) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(src[i])).toArray()));
            i = i - 1;
        }
        return res;
    }

    static int[] patience_sort(int[] collection) {
        int[][] stacks = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < collection.length) {
            int element = collection[i_1];
            int idx = bisect_left(((int[][])(stacks)), element);
            if (idx != stacks.length) {
                int[] stack_1 = ((int[])(stacks[idx]));
stacks[idx] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack_1), java.util.stream.IntStream.of(element)).toArray()));
            } else {
                int[] new_stack = ((int[])(new int[]{element}));
                stacks = ((int[][])(appendObj(stacks, new_stack)));
            }
            i_1 = i_1 + 1;
        }
        i_1 = 0;
        while (i_1 < stacks.length) {
stacks[i_1] = ((int[])(reverse_list(((int[])(stacks[i_1])))));
            i_1 = i_1 + 1;
        }
        int[] indices = ((int[])(new int[]{}));
        i_1 = 0;
        while (i_1 < stacks.length) {
            indices = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(indices), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        int total = 0;
        i_1 = 0;
        while (i_1 < stacks.length) {
            total = total + stacks[i_1].length;
            i_1 = i_1 + 1;
        }
        int[] result = ((int[])(new int[]{}));
        int count = 0;
        while (count < total) {
            int min_val = 0;
            int min_stack = -1;
            int j = 0;
            while (j < stacks.length) {
                int idx_1 = indices[j];
                if (idx_1 < stacks[j].length) {
                    int val = stacks[j][idx_1];
                    if (min_stack < 0) {
                        min_val = val;
                        min_stack = j;
                    } else                     if (val < min_val) {
                        min_val = val;
                        min_stack = j;
                    }
                }
                j = j + 1;
            }
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(min_val)).toArray()));
indices[min_stack] = indices[min_stack] + 1;
            count = count + 1;
        }
        i_1 = 0;
        while (i_1 < result.length) {
collection[i_1] = result[i_1];
            i_1 = i_1 + 1;
        }
        return collection;
    }
    public static void main(String[] args) {
        System.out.println(_p(patience_sort(((int[])(new int[]{1, 9, 5, 21, 17, 6})))));
        System.out.println(_p(patience_sort(((int[])(new int[]{})))));
        System.out.println(_p(patience_sort(((int[])(new int[]{-3, -17, -48})))));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
