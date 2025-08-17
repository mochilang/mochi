public class Main {
    static class Coord {
        long x;
        long y;
        Coord(long x, long y) {
            this.x = x;
            this.y = y;
        }
        Coord() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static class PlayResult {
        String[][] matrix;
        long score;
        PlayResult(String[][] matrix, long score) {
            this.matrix = matrix;
            this.score = score;
        }
        PlayResult() {}
        @Override public String toString() {
            return String.format("{'matrix': %s, 'score': %s}", String.valueOf(matrix), String.valueOf(score));
        }
    }


    static boolean is_alnum(String ch) {
        return ((ch.compareTo("0") >= 0) && (ch.compareTo("9") <= 0)) || ((ch.compareTo("A") >= 0) && (ch.compareTo("Z") <= 0)) || ((ch.compareTo("a") >= 0) && (ch.compareTo("z") <= 0));
    }

    static long to_int(String token) {
        long res = 0L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(token))) {
            res = (long)((long)((long)(res) * 10L) + (long)((Integer.parseInt(_substr(token, (int)((long)(i_1)), (int)((long)((long)(i_1) + 1L)))))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static String[] split(String s, String sep) {
        String[] res_1 = ((String[])(new String[]{}));
        String current_1 = "";
        long i_3 = 0L;
        while ((long)(i_3) < (long)(_runeLen(s))) {
            String ch_1 = _substr(s, (int)((long)(i_3)), (int)((long)((long)(i_3) + 1L)));
            if ((ch_1.equals(sep))) {
                res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + ch_1;
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res_1;
    }

    static Coord[] parse_moves(String input_str) {
        String[] pairs = ((String[])(input_str.split(java.util.regex.Pattern.quote(","))));
        Coord[] moves_1 = ((Coord[])(new Coord[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(pairs.length)) {
            String pair_1 = pairs[(int)((long)(i_5))];
            String[] numbers_1 = ((String[])(new String[]{}));
            String num_1 = "";
            long j_1 = 0L;
            while ((long)(j_1) < (long)(_runeLen(pair_1))) {
                String ch_3 = _substr(pair_1, (int)((long)(j_1)), (int)((long)((long)(j_1) + 1L)));
                if ((ch_3.equals(" "))) {
                    if (!(num_1.equals(""))) {
                        numbers_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers_1), java.util.stream.Stream.of(num_1)).toArray(String[]::new)));
                        num_1 = "";
                    }
                } else {
                    num_1 = num_1 + ch_3;
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            if (!(num_1.equals(""))) {
                numbers_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers_1), java.util.stream.Stream.of(num_1)).toArray(String[]::new)));
            }
            if ((long)(numbers_1.length) != 2L) {
                throw new RuntimeException(String.valueOf("Each move must have exactly two numbers."));
            }
            long x_1 = (long)(to_int(numbers_1[(int)(0L)]));
            long y_1 = (long)(to_int(numbers_1[(int)(1L)]));
            moves_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(moves_1), java.util.stream.Stream.of(new Coord(x_1, y_1))).toArray(Coord[]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return moves_1;
    }

    static void validate_matrix_size(long size) {
        if ((long)(size) <= 0L) {
            throw new RuntimeException(String.valueOf("Matrix size must be a positive integer."));
        }
    }

    static void validate_matrix_content(String[] matrix, long size) {
        if ((long)(matrix.length) != (long)(size)) {
            throw new RuntimeException(String.valueOf("The matrix dont match with size."));
        }
        long i_7 = 0L;
        while ((long)(i_7) < (long)(size)) {
            String row_1 = matrix[(int)((long)(i_7))];
            if ((long)(_runeLen(row_1)) != (long)(size)) {
                throw new RuntimeException(String.valueOf("Each row in the matrix must have exactly " + _p(size) + " characters."));
            }
            long j_3 = 0L;
            while ((long)(j_3) < (long)(size)) {
                String ch_5 = _substr(row_1, (int)((long)(j_3)), (int)((long)((long)(j_3) + 1L)));
                if (!(Boolean)is_alnum(ch_5)) {
                    throw new RuntimeException(String.valueOf("Matrix rows can only contain letters and numbers."));
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
    }

    static void validate_moves(Coord[] moves, long size) {
        long i_8 = 0L;
        while ((long)(i_8) < (long)(moves.length)) {
            Coord mv_1 = moves[(int)((long)(i_8))];
            if ((long)(mv_1.x) < 0L || (long)(mv_1.x) >= (long)(size) || (long)(mv_1.y) < 0L || (long)(mv_1.y) >= (long)(size)) {
                throw new RuntimeException(String.valueOf("Move is out of bounds for a matrix."));
            }
            i_8 = (long)((long)(i_8) + 1L);
        }
    }

    static boolean contains(Coord[] pos, long r, long c) {
        long i_9 = 0L;
        while ((long)(i_9) < (long)(pos.length)) {
            Coord p_1 = pos[(int)((long)(i_9))];
            if ((long)(p_1.x) == (long)(r) && (long)(p_1.y) == (long)(c)) {
                return true;
            }
            i_9 = (long)((long)(i_9) + 1L);
        }
        return false;
    }

    static Coord[] find_repeat(String[][] matrix_g, long row, long column, long size) {
        column = (long)((long)((long)(size) - 1L) - (long)(column));
        Coord[] visited_1 = ((Coord[])(new Coord[]{}));
        Coord[] repeated_1 = ((Coord[])(new Coord[]{}));
        String color_1 = matrix_g[(int)((long)(column))][(int)((long)(row))];
        if ((color_1.equals("-"))) {
            return repeated_1;
        }
        Coord[] stack_1 = ((Coord[])(new Coord[]{new Coord(column, row)}));
        while ((long)(stack_1.length) > 0L) {
            long idx_1 = (long)((long)(stack_1.length) - 1L);
            Coord pos_1 = stack_1[(int)((long)(idx_1))];
            stack_1 = ((Coord[])(java.util.Arrays.copyOfRange(stack_1, (int)(0L), (int)((long)(idx_1)))));
            if ((long)(pos_1.x) < 0L || (long)(pos_1.x) >= (long)(size) || (long)(pos_1.y) < 0L || (long)(pos_1.y) >= (long)(size)) {
                continue;
            }
            if (contains(((Coord[])(visited_1)), (long)(pos_1.x), (long)(pos_1.y))) {
                continue;
            }
            visited_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited_1), java.util.stream.Stream.of(pos_1)).toArray(Coord[]::new)));
            if ((matrix_g[(int)((long)(pos_1.x))][(int)((long)(pos_1.y))].equals(color_1))) {
                repeated_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(repeated_1), java.util.stream.Stream.of(pos_1)).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord((long)(pos_1.x) - 1L, pos_1.y))).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord((long)(pos_1.x) + 1L, pos_1.y))).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord(pos_1.x, (long)(pos_1.y) - 1L))).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord(pos_1.x, (long)(pos_1.y) + 1L))).toArray(Coord[]::new)));
            }
        }
        return repeated_1;
    }

    static long increment_score(long count) {
        return ((long)(Math.floorDiv(((long)((long)(count) * (long)(((long)(count) + 1L)))), ((long)(2)))));
    }

    static String[][] move_x(String[][] matrix_g, long column, long size) {
        String[] new_list = ((String[])(new String[]{}));
        long row_3 = 0L;
        while ((long)(row_3) < (long)(size)) {
            String val_1 = matrix_g[(int)((long)(row_3))][(int)((long)(column))];
            if (!(val_1.equals("-"))) {
                new_list = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_list), java.util.stream.Stream.of(val_1)).toArray(String[]::new)));
            } else {
                new_list = ((String[])(concat(new String[]{val_1}, new_list)));
            }
            row_3 = (long)((long)(row_3) + 1L);
        }
        row_3 = 0L;
        while ((long)(row_3) < (long)(size)) {
matrix_g[(int)((long)(row_3))][(int)((long)(column))] = new_list[(int)((long)(row_3))];
            row_3 = (long)((long)(row_3) + 1L);
        }
        return matrix_g;
    }

    static String[][] move_y(String[][] matrix_g, long size) {
        long[] empty_cols = ((long[])(new long[]{}));
        long column_1 = (long)((long)(size) - 1L);
        while ((long)(column_1) >= 0L) {
            long row_5 = 0L;
            boolean all_empty_1 = true;
            while ((long)(row_5) < (long)(size)) {
                if (!(matrix_g[(int)((long)(row_5))][(int)((long)(column_1))].equals("-"))) {
                    all_empty_1 = false;
                    break;
                }
                row_5 = (long)((long)(row_5) + 1L);
            }
            if (all_empty_1) {
                empty_cols = ((long[])(appendLong(empty_cols, (long)(column_1))));
            }
            column_1 = (long)((long)(column_1) - 1L);
        }
        long i_11 = 0L;
        while ((long)(i_11) < (long)(empty_cols.length)) {
            long col_1 = (long)(empty_cols[(int)((long)(i_11))]);
            long c_1 = (long)((long)(col_1) + 1L);
            while ((long)(c_1) < (long)(size)) {
                long r_2 = 0L;
                while ((long)(r_2) < (long)(size)) {
matrix_g[(int)((long)(r_2))][(int)((long)((long)(c_1) - 1L))] = matrix_g[(int)((long)(r_2))][(int)((long)(c_1))];
                    r_2 = (long)((long)(r_2) + 1L);
                }
                c_1 = (long)((long)(c_1) + 1L);
            }
            long r_3 = 0L;
            while ((long)(r_3) < (long)(size)) {
matrix_g[(int)((long)(r_3))][(int)((long)((long)(size) - 1L))] = "-";
                r_3 = (long)((long)(r_3) + 1L);
            }
            i_11 = (long)((long)(i_11) + 1L);
        }
        return matrix_g;
    }

    static PlayResult play(String[][] matrix_g, long pos_x, long pos_y, long size) {
        Coord[] same_colors = ((Coord[])(find_repeat(((String[][])(matrix_g)), (long)(pos_x), (long)(pos_y), (long)(size))));
        if ((long)(same_colors.length) != 0L) {
            long i_13 = 0L;
            while ((long)(i_13) < (long)(same_colors.length)) {
                Coord p_3 = same_colors[(int)((long)(i_13))];
matrix_g[(int)((long)(p_3.x))][(int)((long)(p_3.y))] = "-";
                i_13 = (long)((long)(i_13) + 1L);
            }
            long column_3 = 0L;
            while ((long)(column_3) < (long)(size)) {
                matrix_g = ((String[][])(move_x(((String[][])(matrix_g)), (long)(column_3), (long)(size))));
                column_3 = (long)((long)(column_3) + 1L);
            }
            matrix_g = ((String[][])(move_y(((String[][])(matrix_g)), (long)(size))));
        }
        long sc_1 = (long)(increment_score((long)(same_colors.length)));
        return new PlayResult(matrix_g, sc_1);
    }

    static String[][] build_matrix(String[] matrix) {
        String[][] res_2 = ((String[][])(new String[][]{}));
        long i_15 = 0L;
        while ((long)(i_15) < (long)(matrix.length)) {
            String row_7 = matrix[(int)((long)(i_15))];
            String[] row_list_1 = ((String[])(new String[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(_runeLen(row_7))) {
                row_list_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_list_1), java.util.stream.Stream.of(_substr(row_7, (int)((long)(j_5)), (int)((long)((long)(j_5) + 1L))))).toArray(String[]::new)));
                j_5 = (long)((long)(j_5) + 1L);
            }
            res_2 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(new String[][]{row_list_1})).toArray(String[][]::new)));
            i_15 = (long)((long)(i_15) + 1L);
        }
        return res_2;
    }

    static long process_game(long size, String[] matrix, Coord[] moves) {
        String[][] game_matrix = ((String[][])(build_matrix(((String[])(matrix)))));
        long total_1 = 0L;
        long i_17 = 0L;
        while ((long)(i_17) < (long)(moves.length)) {
            Coord mv_3 = moves[(int)((long)(i_17))];
            PlayResult res_4 = play(((String[][])(game_matrix)), (long)(mv_3.x), (long)(mv_3.y), (long)(size));
            game_matrix = ((String[][])(res_4.matrix));
            total_1 = (long)((long)(total_1) + (long)(res_4.score));
            i_17 = (long)((long)(i_17) + 1L);
        }
        return total_1;
    }

    static void main() {
        long size = 4L;
        String[] matrix_1 = ((String[])(new String[]{"RRBG", "RBBG", "YYGG", "XYGG"}));
        Coord[] moves_3 = ((Coord[])(parse_moves("0 1,1 1")));
        validate_matrix_size((long)(size));
        validate_matrix_content(((String[])(matrix_1)), (long)(size));
        validate_moves(((Coord[])(moves_3)), (long)(size));
        long score_1 = (long)(process_game((long)(size), ((String[])(matrix_1)), ((Coord[])(moves_3))));
        System.out.println(_p(score_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static Object concat(Object a, Object b) {
        int len1 = java.lang.reflect.Array.getLength(a);
        int len2 = java.lang.reflect.Array.getLength(b);
        Object out = java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), len1 + len2);
        System.arraycopy(a, 0, out, 0, len1);
        System.arraycopy(b, 0, out, len1, len2);
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
