public class Main {
    static Object[] seq;
    static Object[] seq2;

    static void create_state_space_tree(Object[] sequence, Object[] current, int index) {
        if (index == sequence.length) {
            System.out.println(java.util.Arrays.toString(current));
            return;
        }
        create_state_space_tree(((Object[])(sequence)), ((Object[])(current)), index + 1);
        Object[] with_elem = ((Object[])(java.util.stream.Stream.concat(java.util.Arrays.stream(current), java.util.stream.Stream.of(sequence[index])).toArray(Object[]::new)));
        create_state_space_tree(((Object[])(sequence)), ((Object[])(with_elem)), index + 1);
    }

    static void generate_all_subsequences(Object[] sequence) {
        create_state_space_tree(((Object[])(sequence)), ((Object[])(((Object[])(new Object[]{})))), 0);
    }
    public static void main(String[] args) {
        seq = new Object[]{1, 2, 3};
        generate_all_subsequences(((Object[])(seq)));
        seq2 = new Object[]{"A", "B", "C"};
        generate_all_subsequences(((Object[])(seq2)));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
