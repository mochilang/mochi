public class Main {
    static class Coord {
        int x;
        int y;
        Coord(int x, int y) {
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
        int score;
        PlayResult(String[][] matrix, int score) {
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

    static int to_int(String token) {
        int res = 0;
        int i = 0;
        while (i < _runeLen(token)) {
            res = res * 10 + (Integer.parseInt(_substr(token, i, i + 1)));
            i = i + 1;
        }
        return res;
    }

    static String[] split(String s, String sep) {
        String[] res_1 = ((String[])(new String[]{}));
        String current = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            String ch = _substr(s, i_1, i_1 + 1);
            if ((ch.equals(sep))) {
                res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current)).toArray(String[]::new)));
                current = "";
            } else {
                current = current + ch;
            }
            i_1 = i_1 + 1;
        }
        res_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(current)).toArray(String[]::new)));
        return res_1;
    }

    static Coord[] parse_moves(String input_str) {
        String[] pairs = ((String[])(input_str.split(java.util.regex.Pattern.quote(","))));
        Coord[] moves = ((Coord[])(new Coord[]{}));
        int i_2 = 0;
        while (i_2 < pairs.length) {
            String pair = pairs[i_2];
            String[] numbers = ((String[])(new String[]{}));
            String num = "";
            int j = 0;
            while (j < _runeLen(pair)) {
                String ch_1 = _substr(pair, j, j + 1);
                if ((ch_1.equals(" "))) {
                    if (!(num.equals(""))) {
                        numbers = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers), java.util.stream.Stream.of(num)).toArray(String[]::new)));
                        num = "";
                    }
                } else {
                    num = num + ch_1;
                }
                j = j + 1;
            }
            if (!(num.equals(""))) {
                numbers = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(numbers), java.util.stream.Stream.of(num)).toArray(String[]::new)));
            }
            if (numbers.length != 2) {
                throw new RuntimeException(String.valueOf("Each move must have exactly two numbers."));
            }
            int x = to_int(numbers[0]);
            int y = to_int(numbers[1]);
            moves = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(moves), java.util.stream.Stream.of(new Coord(x, y))).toArray(Coord[]::new)));
            i_2 = i_2 + 1;
        }
        return moves;
    }

    static void validate_matrix_size(int size) {
        if (size <= 0) {
            throw new RuntimeException(String.valueOf("Matrix size must be a positive integer."));
        }
    }

    static void validate_matrix_content(String[] matrix, int size) {
        if (matrix.length != size) {
            throw new RuntimeException(String.valueOf("The matrix dont match with size."));
        }
        int i_3 = 0;
        while (i_3 < size) {
            String row = matrix[i_3];
            if (_runeLen(row) != size) {
                throw new RuntimeException(String.valueOf("Each row in the matrix must have exactly " + _p(size) + " characters."));
            }
            int j_1 = 0;
            while (j_1 < size) {
                String ch_2 = _substr(row, j_1, j_1 + 1);
                if (!(Boolean)is_alnum(ch_2)) {
                    throw new RuntimeException(String.valueOf("Matrix rows can only contain letters and numbers."));
                }
                j_1 = j_1 + 1;
            }
            i_3 = i_3 + 1;
        }
    }

    static void validate_moves(Coord[] moves, int size) {
        int i_4 = 0;
        while (i_4 < moves.length) {
            Coord mv = moves[i_4];
            if (mv.x < 0 || mv.x >= size || mv.y < 0 || mv.y >= size) {
                throw new RuntimeException(String.valueOf("Move is out of bounds for a matrix."));
            }
            i_4 = i_4 + 1;
        }
    }

    static boolean contains(Coord[] pos, int r, int c) {
        int i_5 = 0;
        while (i_5 < pos.length) {
            Coord p = pos[i_5];
            if (p.x == r && p.y == c) {
                return true;
            }
            i_5 = i_5 + 1;
        }
        return false;
    }

    static Coord[] find_repeat(String[][] matrix_g, int row, int column, int size) {
        column = size - 1 - column;
        Coord[] visited = ((Coord[])(new Coord[]{}));
        Coord[] repeated = ((Coord[])(new Coord[]{}));
        String color = matrix_g[column][row];
        if ((color.equals("-"))) {
            return repeated;
        }
        Coord[] stack = ((Coord[])(new Coord[]{new Coord(column, row)}));
        while (stack.length > 0) {
            int idx = stack.length - 1;
            Coord pos = stack[idx];
            stack = ((Coord[])(java.util.Arrays.copyOfRange(stack, 0, idx)));
            if (pos.x < 0 || pos.x >= size || pos.y < 0 || pos.y >= size) {
                continue;
            }
            if (((Boolean)(contains(((Coord[])(visited)), pos.x, pos.y)))) {
                continue;
            }
            visited = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited), java.util.stream.Stream.of(pos)).toArray(Coord[]::new)));
            if ((matrix_g[pos.x][pos.y].equals(color))) {
                repeated = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(repeated), java.util.stream.Stream.of(pos)).toArray(Coord[]::new)));
                stack = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(new Coord(pos.x - 1, pos.y))).toArray(Coord[]::new)));
                stack = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(new Coord(pos.x + 1, pos.y))).toArray(Coord[]::new)));
                stack = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(new Coord(pos.x, pos.y - 1))).toArray(Coord[]::new)));
                stack = ((Coord[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(new Coord(pos.x, pos.y + 1))).toArray(Coord[]::new)));
            }
        }
        return repeated;
    }

    static int increment_score(int count) {
        return Math.floorDiv(count * (count + 1), 2);
    }

    static String[][] move_x(String[][] matrix_g, int column, int size) {
        String[] new_list = ((String[])(new String[]{}));
        int row_1 = 0;
        while (row_1 < size) {
            String val = matrix_g[row_1][column];
            if (!(val.equals("-"))) {
                new_list = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_list), java.util.stream.Stream.of(val)).toArray(String[]::new)));
            } else {
                new_list = ((String[])(concat(new String[]{val}, new_list)));
            }
            row_1 = row_1 + 1;
        }
        row_1 = 0;
        while (row_1 < size) {
matrix_g[row_1][column] = new_list[row_1];
            row_1 = row_1 + 1;
        }
        return matrix_g;
    }

    static String[][] move_y(String[][] matrix_g, int size) {
        int[] empty_cols = ((int[])(new int[]{}));
        int column = size - 1;
        while (column >= 0) {
            int row_2 = 0;
            boolean all_empty = true;
            while (row_2 < size) {
                if (!(matrix_g[row_2][column].equals("-"))) {
                    all_empty = false;
                    break;
                }
                row_2 = row_2 + 1;
            }
            if (all_empty) {
                empty_cols = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(empty_cols), java.util.stream.IntStream.of(column)).toArray()));
            }
            column = column - 1;
        }
        int i_6 = 0;
        while (i_6 < empty_cols.length) {
            int col = empty_cols[i_6];
            int c = col + 1;
            while (c < size) {
                int r = 0;
                while (r < size) {
matrix_g[r][c - 1] = matrix_g[r][c];
                    r = r + 1;
                }
                c = c + 1;
            }
            int r_1 = 0;
            while (r_1 < size) {
matrix_g[r_1][size - 1] = "-";
                r_1 = r_1 + 1;
            }
            i_6 = i_6 + 1;
        }
        return matrix_g;
    }

    static PlayResult play(String[][] matrix_g, int pos_x, int pos_y, int size) {
        Coord[] same_colors = ((Coord[])(find_repeat(((String[][])(matrix_g)), pos_x, pos_y, size)));
        if (same_colors.length != 0) {
            int i_7 = 0;
            while (i_7 < same_colors.length) {
                Coord p_1 = same_colors[i_7];
matrix_g[p_1.x][p_1.y] = "-";
                i_7 = i_7 + 1;
            }
            int column_1 = 0;
            while (column_1 < size) {
                matrix_g = ((String[][])(move_x(((String[][])(matrix_g)), column_1, size)));
                column_1 = column_1 + 1;
            }
            matrix_g = ((String[][])(move_y(((String[][])(matrix_g)), size)));
        }
        int sc = increment_score(same_colors.length);
        return new PlayResult(matrix_g, sc);
    }

    static String[][] build_matrix(String[] matrix) {
        String[][] res_2 = ((String[][])(new String[][]{}));
        int i_8 = 0;
        while (i_8 < matrix.length) {
            String row_3 = matrix[i_8];
            String[] row_list = ((String[])(new String[]{}));
            int j_2 = 0;
            while (j_2 < _runeLen(row_3)) {
                row_list = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_list), java.util.stream.Stream.of(_substr(row_3, j_2, j_2 + 1))).toArray(String[]::new)));
                j_2 = j_2 + 1;
            }
            res_2 = ((String[][])(appendObj(res_2, row_list)));
            i_8 = i_8 + 1;
        }
        return res_2;
    }

    static int process_game(int size, String[] matrix, Coord[] moves) {
        String[][] game_matrix = ((String[][])(build_matrix(((String[])(matrix)))));
        int total = 0;
        int i_9 = 0;
        while (i_9 < moves.length) {
            Coord mv_1 = moves[i_9];
            PlayResult res_3 = play(((String[][])(game_matrix)), mv_1.x, mv_1.y, size);
            game_matrix = ((String[][])(res_3.matrix));
            total = total + res_3.score;
            i_9 = i_9 + 1;
        }
        return total;
    }

    static void main() {
        int size = 4;
        String[] matrix = ((String[])(new String[]{"RRBG", "RBBG", "YYGG", "XYGG"}));
        Coord[] moves_1 = ((Coord[])(parse_moves("0 1,1 1")));
        validate_matrix_size(size);
        validate_matrix_content(((String[])(matrix)), size);
        validate_moves(((Coord[])(moves_1)), size);
        int score = process_game(size, ((String[])(matrix)), ((Coord[])(moves_1)));
        System.out.println(_p(score));
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

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
