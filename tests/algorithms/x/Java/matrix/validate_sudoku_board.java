public class Main {
    static long NUM_SQUARES = 9L;
    static String EMPTY_CELL = ".";
    static String[][] valid_board = ((String[][])(new String[][]{new String[]{"5", "3", ".", ".", "7", ".", ".", ".", "."}, new String[]{"6", ".", ".", "1", "9", "5", ".", ".", "."}, new String[]{".", "9", "8", ".", ".", ".", ".", "6", "."}, new String[]{"8", ".", ".", ".", "6", ".", ".", ".", "3"}, new String[]{"4", ".", ".", "8", ".", "3", ".", ".", "1"}, new String[]{"7", ".", ".", ".", "2", ".", ".", ".", "6"}, new String[]{".", "6", ".", ".", ".", ".", "2", "8", "."}, new String[]{".", ".", ".", "4", "1", "9", ".", ".", "5"}, new String[]{".", ".", ".", ".", "8", ".", ".", "7", "9"}}));
    static String[][] invalid_board = ((String[][])(new String[][]{new String[]{"8", "3", ".", ".", "7", ".", ".", ".", "."}, new String[]{"6", ".", ".", "1", "9", "5", ".", ".", "."}, new String[]{".", "9", "8", ".", ".", ".", ".", "6", "."}, new String[]{"8", ".", ".", ".", "6", ".", ".", ".", "3"}, new String[]{"4", ".", ".", "8", ".", "3", ".", ".", "1"}, new String[]{"7", ".", ".", ".", "2", ".", ".", ".", "6"}, new String[]{".", "6", ".", ".", ".", ".", "2", "8", "."}, new String[]{".", ".", ".", "4", "1", "9", ".", ".", "5"}, new String[]{".", ".", ".", ".", "8", ".", ".", "7", "9"}}));

    static boolean is_valid_sudoku_board(String[][] board) {
        if ((long)(board.length) != (long)(NUM_SQUARES)) {
            return false;
        }
        long i_1 = 0L;
        while ((long)(i_1) < (long)(NUM_SQUARES)) {
            if ((long)(board[(int)((long)(i_1))].length) != (long)(NUM_SQUARES)) {
                return false;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        String[][] rows_1 = ((String[][])(new String[][]{}));
        String[][] cols_1 = ((String[][])(new String[][]{}));
        String[][] boxes_1 = ((String[][])(new String[][]{}));
        i_1 = 0L;
        while ((long)(i_1) < (long)(NUM_SQUARES)) {
            rows_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(rows_1), java.util.stream.Stream.of(new String[][]{new String[]{}})).toArray(String[][]::new)));
            cols_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(cols_1), java.util.stream.Stream.of(new String[][]{new String[]{}})).toArray(String[][]::new)));
            boxes_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(boxes_1), java.util.stream.Stream.of(new String[][]{new String[]{}})).toArray(String[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        for (int r = 0; r < NUM_SQUARES; r++) {
            for (int c = 0; c < NUM_SQUARES; c++) {
                String value_1 = board[(int)((long)(r))][(int)((long)(c))];
                if ((value_1.equals(EMPTY_CELL))) {
                    continue;
                }
                long box_1 = (long)((long)((long)(((Number)((long)(r) / 3L)).intValue()) * 3L) + (long)(((Number)((long)(c) / 3L)).intValue()));
                if (java.util.Arrays.asList(rows_1[(int)((long)(r))]).contains(value_1) || java.util.Arrays.asList(cols_1[(int)((long)(c))]).contains(value_1) || java.util.Arrays.asList(boxes_1[(int)((long)(box_1))]).contains(value_1)) {
                    return false;
                }
rows_1[(int)((long)(r))] = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rows_1[(int)((long)(r))]), java.util.stream.Stream.of(value_1)).toArray(String[]::new)));
cols_1[(int)((long)(c))] = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cols_1[(int)((long)(c))]), java.util.stream.Stream.of(value_1)).toArray(String[]::new)));
boxes_1[(int)((long)(box_1))] = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(boxes_1[(int)((long)(box_1))]), java.util.stream.Stream.of(value_1)).toArray(String[]::new)));
            }
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(is_valid_sudoku_board(((String[][])(valid_board))));
            System.out.println(is_valid_sudoku_board(((String[][])(invalid_board))));
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
}
