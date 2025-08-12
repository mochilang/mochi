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
            res = (long)((long)((long)(res) * (long)(10)) + (long)((Integer.parseInt(_substr(token, (int)((long)(i_1)), (int)((long)((long)(i_1) + (long)(1))))))));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return res;
    }

    static String[] split(String s, String sep) {
        String[] res_1 = ((String[])(new String[]{}));
        String current_1 = "";
        long i_3 = 0L;
        while ((long)(i_3) < (long)(_runeLen(s))) {
            String ch_1 = _substr(s, (int)((long)(i_3)), (int)((long)((long)(i_3) + (long)(1))));
            if ((ch_1.equals(sep))) {
                res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
                current_1 = "";
            } else {
                current_1 = current_1 + ch_1;
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current_1)).toArray(String[]::new)));
        return res_1;
    }

    static Coord[] parse_moves(String input_str) {
        String[] pairs = ((String[])(input_str.split(java.util.regex.Pattern.quote(","))));
        Coord[] moves_1 = ((Coord[])(new Coord[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(pairs.length)) {
            String pair_1 = ((String)_geto(pairs, (int)((long)(i_5))));
            String[] numbers_1 = ((String[])(new String[]{}));
            String num_1 = "";
            long j_1 = 0L;
            while ((long)(j_1) < (long)(_runeLen(pair_1))) {
                String ch_3 = _substr(pair_1, (int)((long)(j_1)), (int)((long)((long)(j_1) + (long)(1))));
                if ((ch_3.equals(" "))) {
                    if (!(num_1.equals(""))) {
                        numbers_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers_1), java.util.stream.Stream.of(num_1)).toArray(String[]::new)));
                        num_1 = "";
                    }
                } else {
                    num_1 = num_1 + ch_3;
                }
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            if (!(num_1.equals(""))) {
                numbers_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers_1), java.util.stream.Stream.of(num_1)).toArray(String[]::new)));
            }
            if ((long)(numbers_1.length) != (long)(2)) {
                throw new RuntimeException(String.valueOf("Each move must have exactly two numbers."));
            }
            long x_1 = to_int(((String)_geto(numbers_1, (int)((long)(0)))));
            long y_1 = to_int(((String)_geto(numbers_1, (int)((long)(1)))));
            moves_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(moves_1), java.util.stream.Stream.of(new Coord(x_1, y_1))).toArray(Coord[]::new)));
            i_5 = (long)((long)(i_5) + (long)(1));
        }
        return moves_1;
    }

    static void validate_matrix_size(long size) {
        if (size <= (long)(0)) {
            throw new RuntimeException(String.valueOf("Matrix size must be a positive integer."));
        }
    }

    static void validate_matrix_content(String[] matrix, long size) {
        if ((long)(matrix.length) != size) {
            throw new RuntimeException(String.valueOf("The matrix dont match with size."));
        }
        long i_7 = 0L;
        while ((long)(i_7) < size) {
            String row_1 = ((String)_geto(matrix, (int)((long)(i_7))));
            if ((long)(_runeLen(row_1)) != size) {
                throw new RuntimeException(String.valueOf("Each row in the matrix must have exactly " + _p(size) + " characters."));
            }
            long j_3 = 0L;
            while ((long)(j_3) < size) {
                String ch_5 = _substr(row_1, (int)((long)(j_3)), (int)((long)((long)(j_3) + (long)(1))));
                if (!(Boolean)is_alnum(ch_5)) {
                    throw new RuntimeException(String.valueOf("Matrix rows can only contain letters and numbers."));
                }
                j_3 = (long)((long)(j_3) + (long)(1));
            }
            i_7 = (long)((long)(i_7) + (long)(1));
        }
    }

    static void validate_moves(Coord[] moves, long size) {
        long i_8 = 0L;
        while ((long)(i_8) < (long)(moves.length)) {
            Coord mv_1 = ((Coord)_geto(moves, (int)((long)(i_8))));
            if ((long)(mv_1.x) < (long)(0) || (long)(mv_1.x) >= size || (long)(mv_1.y) < (long)(0) || (long)(mv_1.y) >= size) {
                throw new RuntimeException(String.valueOf("Move is out of bounds for a matrix."));
            }
            i_8 = (long)((long)(i_8) + (long)(1));
        }
    }

    static boolean contains(Coord[] pos, long r, long c) {
        long i_9 = 0L;
        while ((long)(i_9) < (long)(pos.length)) {
            Coord p_1 = ((Coord)_geto(pos, (int)((long)(i_9))));
            if ((long)(p_1.x) == r && (long)(p_1.y) == c) {
                return true;
            }
            i_9 = (long)((long)(i_9) + (long)(1));
        }
        return false;
    }

    static Coord[] find_repeat(String[][] matrix_g, long row, long column, long size) {
        column = (long)((long)(size - (long)(1)) - column);
        Coord[] visited_1 = ((Coord[])(new Coord[]{}));
        Coord[] repeated_1 = ((Coord[])(new Coord[]{}));
        String color_1 = ((String)_geto(((String[])_geto(matrix_g, (int)((long)(column)))), (int)((long)(row))));
        if ((color_1.equals("-"))) {
            return repeated_1;
        }
        Coord[] stack_1 = ((Coord[])(new Coord[]{new Coord(column, row)}));
        while ((long)(stack_1.length) > (long)(0)) {
            long idx_1 = (long)((long)(stack_1.length) - (long)(1));
            Coord pos_1 = ((Coord)_geto(stack_1, (int)((long)(idx_1))));
            stack_1 = ((Coord[])(java.util.Arrays.copyOfRange(stack_1, (int)((long)(0)), (int)((long)(idx_1)))));
            if ((long)(pos_1.x) < (long)(0) || (long)(pos_1.x) >= size || (long)(pos_1.y) < (long)(0) || (long)(pos_1.y) >= size) {
                continue;
            }
            if (contains(((Coord[])(visited_1)), (long)(pos_1.x), (long)(pos_1.y))) {
                continue;
            }
            visited_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited_1), java.util.stream.Stream.of(pos_1)).toArray(Coord[]::new)));
            if ((((String)_geto(((String[])_geto(matrix_g, (int)((long)(pos_1.x)))), (int)((long)(pos_1.y)))).equals(color_1))) {
                repeated_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(repeated_1), java.util.stream.Stream.of(pos_1)).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord((long)(pos_1.x) - (long)(1), pos_1.y))).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord((long)(pos_1.x) + (long)(1), pos_1.y))).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord(pos_1.x, (long)(pos_1.y) - (long)(1)))).toArray(Coord[]::new)));
                stack_1 = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(new Coord(pos_1.x, (long)(pos_1.y) + (long)(1)))).toArray(Coord[]::new)));
            }
        }
        return repeated_1;
    }

    static long increment_score(long count) {
        return ((long)(Math.floorDiv(count * (long)((count + (long)(1))), 2)));
    }

    static String[][] move_x(String[][] matrix_g, long column, long size) {
        String[] new_list = ((String[])(new String[]{}));
        long row_3 = 0L;
        while ((long)(row_3) < size) {
            String val_1 = ((String)_geto(((String[])_geto(matrix_g, (int)((long)(row_3)))), (int)((long)(column))));
            if (!(val_1.equals("-"))) {
                new_list = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_list), java.util.stream.Stream.of(val_1)).toArray(String[]::new)));
            } else {
                new_list = ((String[])(concat(new String[]{val_1}, new_list)));
            }
            row_3 = (long)((long)(row_3) + (long)(1));
        }
        row_3 = (long)(0);
        while ((long)(row_3) < size) {
((String[])_geto(matrix_g, (int)((long)(row_3))))[(int)((long)(column))] = ((String)_geto(new_list, (int)((long)(row_3))));
            row_3 = (long)((long)(row_3) + (long)(1));
        }
        return matrix_g;
    }

    static String[][] move_y(String[][] matrix_g, long size) {
        long[] empty_cols = ((long[])(new long[]{}));
        long column_1 = (long)(size - (long)(1));
        while ((long)(column_1) >= (long)(0)) {
            long row_5 = 0L;
            boolean all_empty_1 = true;
            while ((long)(row_5) < size) {
                if (!(((String)_geto(((String[])_geto(matrix_g, (int)((long)(row_5)))), (int)((long)(column_1)))).equals("-"))) {
                    all_empty_1 = false;
                    break;
                }
                row_5 = (long)((long)(row_5) + (long)(1));
            }
            if (all_empty_1) {
                empty_cols = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(empty_cols), java.util.stream.LongStream.of((long)(column_1))).toArray()));
            }
            column_1 = (long)((long)(column_1) - (long)(1));
        }
        long i_11 = 0L;
        while ((long)(i_11) < (long)(empty_cols.length)) {
            long col_1 = _geti(empty_cols, (int)((long)(i_11)));
            long c_1 = (long)(col_1 + (long)(1));
            while ((long)(c_1) < size) {
                long r_2 = 0L;
                while ((long)(r_2) < size) {
((String[])_geto(matrix_g, (int)((long)(r_2))))[(int)((long)((long)(c_1) - (long)(1)))] = ((String)_geto(((String[])_geto(matrix_g, (int)((long)(r_2)))), (int)((long)(c_1))));
                    r_2 = (long)((long)(r_2) + (long)(1));
                }
                c_1 = (long)((long)(c_1) + (long)(1));
            }
            long r_3 = 0L;
            while ((long)(r_3) < size) {
((String[])_geto(matrix_g, (int)((long)(r_3))))[(int)((long)(size - (long)(1)))] = "-";
                r_3 = (long)((long)(r_3) + (long)(1));
            }
            i_11 = (long)((long)(i_11) + (long)(1));
        }
        return matrix_g;
    }

    static PlayResult play(String[][] matrix_g, long pos_x, long pos_y, long size) {
        Coord[] same_colors = ((Coord[])(find_repeat(((String[][])(matrix_g)), pos_x, pos_y, size)));
        if ((long)(same_colors.length) != (long)(0)) {
            long i_13 = 0L;
            while ((long)(i_13) < (long)(same_colors.length)) {
                Coord p_3 = ((Coord)_geto(same_colors, (int)((long)(i_13))));
((String[])_geto(matrix_g, (int)((long)(p_3.x))))[(int)((long)(p_3.y))] = "-";
                i_13 = (long)((long)(i_13) + (long)(1));
            }
            long column_3 = 0L;
            while ((long)(column_3) < size) {
                matrix_g = ((String[][])(move_x(((String[][])(matrix_g)), (long)(column_3), size)));
                column_3 = (long)((long)(column_3) + (long)(1));
            }
            matrix_g = ((String[][])(move_y(((String[][])(matrix_g)), size)));
        }
        long sc_1 = increment_score((long)(same_colors.length));
        return new PlayResult(matrix_g, sc_1);
    }

    static String[][] build_matrix(String[] matrix) {
        String[][] res_2 = ((String[][])(new String[][]{}));
        long i_15 = 0L;
        while ((long)(i_15) < (long)(matrix.length)) {
            String row_7 = ((String)_geto(matrix, (int)((long)(i_15))));
            String[] row_list_1 = ((String[])(new String[]{}));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(_runeLen(row_7))) {
                row_list_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_list_1), java.util.stream.Stream.of(_substr(row_7, (int)((long)(j_5)), (int)((long)((long)(j_5) + (long)(1)))))).toArray(String[]::new)));
                j_5 = (long)((long)(j_5) + (long)(1));
            }
            res_2 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_2), java.util.stream.Stream.of(row_list_1)).toArray(String[][]::new)));
            i_15 = (long)((long)(i_15) + (long)(1));
        }
        return res_2;
    }

    static long process_game(long size, String[] matrix, Coord[] moves) {
        String[][] game_matrix = ((String[][])(build_matrix(((String[])(matrix)))));
        long total_1 = 0L;
        long i_17 = 0L;
        while ((long)(i_17) < (long)(moves.length)) {
            Coord mv_3 = ((Coord)_geto(moves, (int)((long)(i_17))));
            PlayResult res_4 = play(((String[][])(game_matrix)), (long)(mv_3.x), (long)(mv_3.y), size);
            game_matrix = ((String[][])(res_4.matrix));
            total_1 = (long)((long)(total_1) + (long)(res_4.score));
            i_17 = (long)((long)(i_17) + (long)(1));
        }
        return total_1;
    }

    static void main() {
        long size = (long)(4);
        String[] matrix_1 = ((String[])(new String[]{"RRBG", "RBBG", "YYGG", "XYGG"}));
        Coord[] moves_3 = ((Coord[])(parse_moves("0 1,1 1")));
        validate_matrix_size((long)(size));
        validate_matrix_content(((String[])(matrix_1)), (long)(size));
        validate_moves(((Coord[])(moves_3)), (long)(size));
        long score_1 = process_game((long)(size), ((String[])(matrix_1)), ((Coord[])(moves_3)));
        System.out.println(_p(score_1));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static long _geti(long[] a, int i) {
        if (a == null) return 0L;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0L;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
