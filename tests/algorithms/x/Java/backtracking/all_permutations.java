public class Main {
    static Object[] sequence;
    static Object[] sequence_2;

    static boolean[] repeat_bool(int times) {
        boolean[] res = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i < times) {
            res = ((boolean[])(appendBool(res, false)));
            i = i + 1;
        }
        return res;
    }

    static boolean[] set_bool(boolean[] xs, int idx, boolean value) {
        boolean[] res_1 = ((boolean[])(new boolean[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 == idx) {
                res_1 = ((boolean[])(appendBool(res_1, ((Boolean)(value)))));
            } else {
                res_1 = ((boolean[])(appendBool(res_1, ((Boolean)(xs[i_1])))));
            }
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static void create_state_space_tree(Object[] sequence, Object[] current, int index, boolean[] used) {
        if (index == sequence.length) {
            System.out.println(_p(current));
            return;
        }
        int i_2 = 0;
        while (i_2 < sequence.length) {
            if (!(Boolean)used[i_2]) {
                Object[] next_current = ((Object[])(java.util.stream.Stream.concat(java.util.Arrays.stream(current), java.util.stream.Stream.of(sequence[i_2])).toArray(Object[]::new)));
                boolean[] next_used = ((boolean[])(set_bool(((boolean[])(used)), i_2, true)));
                create_state_space_tree(((Object[])(sequence)), ((Object[])(next_current)), index + 1, ((boolean[])(next_used)));
            }
            i_2 = i_2 + 1;
        }
    }

    static void generate_all_permutations(Object[] sequence) {
        boolean[] used = ((boolean[])(repeat_bool(sequence.length)));
        create_state_space_tree(((Object[])(sequence)), ((Object[])(new Object[]{})), 0, ((boolean[])(used)));
    }
    public static void main(String[] args) {
        sequence = new Object[]{3, 1, 2, 4};
        generate_all_permutations(((Object[])(sequence)));
        sequence_2 = new Object[]{"A", "B", "C"};
        generate_all_permutations(((Object[])(sequence_2)));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
