public class Main {
    static int NUM_SQUARES;
    static String EMPTY_CELL;
    static String[][] valid_board;
    static String[][] invalid_board;

    static boolean is_valid_sudoku_board(String[][] board) {
        if (board.length != NUM_SQUARES) {
            return false;
        }
        int i = 0;
        while (i < NUM_SQUARES) {
            if (board[i].length != NUM_SQUARES) {
                return false;
            }
            i = i + 1;
        }
        String[][] rows = ((String[][])(new String[][]{}));
        String[][] cols = ((String[][])(new String[][]{}));
        String[][] boxes = ((String[][])(new String[][]{}));
        i = 0;
        while (i < NUM_SQUARES) {
            rows = ((String[][])(appendObj(rows, new String[]{})));
            cols = ((String[][])(appendObj(cols, new String[]{})));
            boxes = ((String[][])(appendObj(boxes, new String[]{})));
            i = i + 1;
        }
        for (int r = 0; r < NUM_SQUARES; r++) {
            for (int c = 0; c < NUM_SQUARES; c++) {
                String value = board[r][c];
                if ((value.equals(EMPTY_CELL))) {
                    continue;
                }
                int box = ((Number)(Math.floorDiv(r, 3))).intValue() * 3 + ((Number)(Math.floorDiv(c, 3))).intValue();
                if (java.util.Arrays.asList(rows[r]).contains(value) || java.util.Arrays.asList(cols[c]).contains(value) || java.util.Arrays.asList(boxes[box]).contains(value)) {
                    return false;
                }
rows[r] = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rows[r]), java.util.stream.Stream.of(value)).toArray(String[]::new)));
cols[c] = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cols[c]), java.util.stream.Stream.of(value)).toArray(String[]::new)));
boxes[box] = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(boxes[box]), java.util.stream.Stream.of(value)).toArray(String[]::new)));
            }
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NUM_SQUARES = 9;
            EMPTY_CELL = ".";
            valid_board = ((String[][])(new String[][]{new String[]{"5", "3", ".", ".", "7", ".", ".", ".", "."}, new String[]{"6", ".", ".", "1", "9", "5", ".", ".", "."}, new String[]{".", "9", "8", ".", ".", ".", ".", "6", "."}, new String[]{"8", ".", ".", ".", "6", ".", ".", ".", "3"}, new String[]{"4", ".", ".", "8", ".", "3", ".", ".", "1"}, new String[]{"7", ".", ".", ".", "2", ".", ".", ".", "6"}, new String[]{".", "6", ".", ".", ".", ".", "2", "8", "."}, new String[]{".", ".", ".", "4", "1", "9", ".", ".", "5"}, new String[]{".", ".", ".", ".", "8", ".", ".", "7", "9"}}));
            invalid_board = ((String[][])(new String[][]{new String[]{"8", "3", ".", ".", "7", ".", ".", ".", "."}, new String[]{"6", ".", ".", "1", "9", "5", ".", ".", "."}, new String[]{".", "9", "8", ".", ".", ".", ".", "6", "."}, new String[]{"8", ".", ".", ".", "6", ".", ".", ".", "3"}, new String[]{"4", ".", ".", "8", ".", "3", ".", ".", "1"}, new String[]{"7", ".", ".", ".", "2", ".", ".", ".", "6"}, new String[]{".", "6", ".", ".", ".", ".", "2", "8", "."}, new String[]{".", ".", ".", "4", "1", "9", ".", ".", "5"}, new String[]{".", ".", ".", ".", "8", ".", ".", "7", "9"}}));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
