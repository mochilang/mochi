public class Main {

    static int sum_list(int[] nums) {
        int s = 0;
        for (int n : nums) {
            s = s + n;
        }
        return s;
    }

    static int[][] create_state_space_tree(int[] nums, int max_sum, int num_index, int[] path, int curr_sum, int remaining_sum) {
        int[][] result = ((int[][])(new int[][]{}));
        if (curr_sum > max_sum || curr_sum + remaining_sum < max_sum) {
            return result;
        }
        if (curr_sum == max_sum) {
            result = ((int[][])(appendObj(result, path)));
            return result;
        }
        int index = num_index;
        while (index < nums.length) {
            int value = nums[index];
            int[][] subres = ((int[][])(create_state_space_tree(((int[])(nums)), max_sum, index + 1, ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(path), java.util.stream.IntStream.of(value)).toArray())), curr_sum + value, remaining_sum - value)));
            int j = 0;
            while (j < subres.length) {
                result = ((int[][])(appendObj(result, subres[j])));
                j = j + 1;
            }
            index = index + 1;
        }
        return result;
    }

    static int[][] generate_sum_of_subsets_solutions(int[] nums, int max_sum) {
        int total = sum_list(((int[])(nums)));
        return create_state_space_tree(((int[])(nums)), max_sum, 0, ((int[])(new int[]{})), 0, total);
    }

    static void main() {
        json(generate_sum_of_subsets_solutions(((int[])(new int[]{3, 34, 4, 12, 5, 2})), 9));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
    }
}
