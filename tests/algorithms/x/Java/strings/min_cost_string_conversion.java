public class Main {
    static class TransformTables {
        int[][] costs;
        String[][] ops;
        TransformTables(int[][] costs, String[][] ops) {
            this.costs = costs;
            this.ops = ops;
        }
        TransformTables() {}
        @Override public String toString() {
            return String.format("{'costs': %s, 'ops': %s}", String.valueOf(costs), String.valueOf(ops));
        }
    }


    static String[] string_to_chars(String s) {
        String[] chars = ((String[])(new String[]{}));
        int i = 0;
        while (i < _runeLen(s)) {
            chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(_substr(s, i, i + 1))).toArray(String[]::new)));
            i = i + 1;
        }
        return chars;
    }

    static String join_chars(String[] chars) {
        String res = "";
        int i_1 = 0;
        while (i_1 < chars.length) {
            res = res + chars[i_1];
            i_1 = i_1 + 1;
        }
        return res;
    }

    static String[] insert_at(String[] chars, int index, String ch) {
        String[] res_1 = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < index) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(chars[i_2])).toArray(String[]::new)));
            i_2 = i_2 + 1;
        }
        res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(ch)).toArray(String[]::new)));
        while (i_2 < chars.length) {
            res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(chars[i_2])).toArray(String[]::new)));
            i_2 = i_2 + 1;
        }
        return res_1;
    }

    static String[] remove_at(String[] chars, int index) {
        String[] res_2 = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < chars.length) {
            if (i_3 != index) {
                res_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(chars[i_3])).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        return res_2;
    }

    static int[][] make_matrix_int(int rows, int cols, int init) {
        int[][] matrix = ((int[][])(new int[][]{}));
        for (int _v = 0; _v < rows; _v++) {
            int[] row = ((int[])(new int[]{}));
            for (int _2 = 0; _2 < cols; _2++) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(init)).toArray()));
            }
            matrix = ((int[][])(appendObj(matrix, row)));
        }
        return matrix;
    }

    static String[][] make_matrix_string(int rows, int cols, String init) {
        String[][] matrix_1 = ((String[][])(new String[][]{}));
        for (int _v = 0; _v < rows; _v++) {
            String[] row_1 = ((String[])(new String[]{}));
            for (int _2 = 0; _2 < cols; _2++) {
                row_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(init)).toArray(String[]::new)));
            }
            matrix_1 = ((String[][])(appendObj(matrix_1, row_1)));
        }
        return matrix_1;
    }

    static TransformTables compute_transform_tables(String source_string, String destination_string, int copy_cost, int replace_cost, int delete_cost, int insert_cost) {
        String[] source_seq = ((String[])(string_to_chars(source_string)));
        String[] dest_seq = ((String[])(string_to_chars(destination_string)));
        int m = source_seq.length;
        int n = dest_seq.length;
        int[][] costs = ((int[][])(make_matrix_int(m + 1, n + 1, 0)));
        String[][] ops = ((String[][])(make_matrix_string(m + 1, n + 1, "0")));
        int i_4 = 1;
        while (i_4 <= m) {
costs[i_4][0] = i_4 * delete_cost;
ops[i_4][0] = "D" + source_seq[i_4 - 1];
            i_4 = i_4 + 1;
        }
        int j = 1;
        while (j <= n) {
costs[0][j] = j * insert_cost;
ops[0][j] = "I" + dest_seq[j - 1];
            j = j + 1;
        }
        i_4 = 1;
        while (i_4 <= m) {
            j = 1;
            while (j <= n) {
                if ((source_seq[i_4 - 1].equals(dest_seq[j - 1]))) {
costs[i_4][j] = costs[i_4 - 1][j - 1] + copy_cost;
ops[i_4][j] = "C" + source_seq[i_4 - 1];
                } else {
costs[i_4][j] = costs[i_4 - 1][j - 1] + replace_cost;
ops[i_4][j] = "R" + source_seq[i_4 - 1] + dest_seq[j - 1];
                }
                if (costs[i_4 - 1][j] + delete_cost < costs[i_4][j]) {
costs[i_4][j] = costs[i_4 - 1][j] + delete_cost;
ops[i_4][j] = "D" + source_seq[i_4 - 1];
                }
                if (costs[i_4][j - 1] + insert_cost < costs[i_4][j]) {
costs[i_4][j] = costs[i_4][j - 1] + insert_cost;
ops[i_4][j] = "I" + dest_seq[j - 1];
                }
                j = j + 1;
            }
            i_4 = i_4 + 1;
        }
        return new TransformTables(costs, ops);
    }

    static String[] assemble_transformation(String[][] ops, int i, int j) {
        if (i == 0 && j == 0) {
            return new String[]{};
        }
        String op = ops[i][j];
        String kind = _substr(op, 0, 1);
        if ((kind.equals("C")) || (kind.equals("R"))) {
            String[] seq = ((String[])(assemble_transformation(((String[][])(ops)), i - 1, j - 1)));
            seq = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(seq), java.util.stream.Stream.of(op)).toArray(String[]::new)));
            return seq;
        } else         if ((kind.equals("D"))) {
            String[] seq_1 = ((String[])(assemble_transformation(((String[][])(ops)), i - 1, j)));
            seq_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(seq_1), java.util.stream.Stream.of(op)).toArray(String[]::new)));
            return seq_1;
        } else {
            String[] seq_2 = ((String[])(assemble_transformation(((String[][])(ops)), i, j - 1)));
            seq_2 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(seq_2), java.util.stream.Stream.of(op)).toArray(String[]::new)));
            return seq_2;
        }
    }

    static void main() {
        int copy_cost = -1;
        int replace_cost = 1;
        int delete_cost = 2;
        int insert_cost = 2;
        String src = "Python";
        String dst = "Algorithms";
        TransformTables tables = compute_transform_tables(src, dst, copy_cost, replace_cost, delete_cost, insert_cost);
        String[][] operations = ((String[][])(tables.ops));
        int m_1 = operations.length;
        int n_1 = operations[0].length;
        String[] sequence = ((String[])(assemble_transformation(((String[][])(operations)), m_1 - 1, n_1 - 1)));
        String[] string_list = ((String[])(string_to_chars(src)));
        int idx = 0;
        int cost = 0;
        int k = 0;
        while (k < sequence.length) {
            System.out.println(join_chars(((String[])(string_list))));
            String op_1 = sequence[k];
            String kind_1 = _substr(op_1, 0, 1);
            if ((kind_1.equals("C"))) {
                cost = cost + copy_cost;
            } else             if ((kind_1.equals("R"))) {
string_list[idx] = _substr(op_1, 2, 3);
                cost = cost + replace_cost;
            } else             if ((kind_1.equals("D"))) {
                string_list = ((String[])(remove_at(((String[])(string_list)), idx)));
                cost = cost + delete_cost;
            } else {
                string_list = ((String[])(insert_at(((String[])(string_list)), idx, _substr(op_1, 1, 2))));
                cost = cost + insert_cost;
            }
            idx = idx + 1;
            k = k + 1;
        }
        System.out.println(join_chars(((String[])(string_list))));
        System.out.println("Cost: " + _p(cost));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
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
