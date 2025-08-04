public class Main {
    static int rows;
    static int cols;
    static double p;
    static double f;
    static String[][] board;

    static String repeat(String ch, int n) {
        String s = "";
        int i = 0;
        while (i < n) {
            s = s + ch;
            i = i + 1;
        }
        return s;
    }

    static boolean chance(double prob) {
        int threshold = ((Number)(prob * 1000.0)).intValue();
        return Math.floorMod(_now(), 1000) < threshold;
    }

    static String[][] newBoard() {
        String[][] b = ((String[][])(new String[][]{}));
        int r = 0;
        while (r < rows) {
            String[] row = ((String[])(new String[]{}));
            int c = 0;
            while (c < cols) {
                if (Math.floorMod(_now(), 2) == 0) {
                    row = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of("T")).toArray(String[]::new)));
                } else {
                    row = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.stream.Stream.of(" ")).toArray(String[]::new)));
                }
                c = c + 1;
            }
            b = ((String[][])(appendObj(b, row)));
            r = r + 1;
        }
        return b;
    }

    static String[][] step(String[][] src) {
        String[][] dst = ((String[][])(new String[][]{}));
        int r_1 = 0;
        while (r_1 < rows) {
            String[] row_1 = ((String[])(new String[]{}));
            int c_1 = 0;
            while (c_1 < cols) {
                String cell = src[r_1][c_1];
                String next = cell;
                if ((cell.equals("#"))) {
                    next = " ";
                } else                 if ((cell.equals("T"))) {
                    boolean burning = false;
                    int dr = -1;
                    while (dr <= 1) {
                        int dc = -1;
                        while (dc <= 1) {
                            if (dr != 0 || dc != 0) {
                                int rr = r_1 + dr;
                                int cc = c_1 + dc;
                                if (rr >= 0 && rr < rows && cc >= 0 && cc < cols) {
                                    if ((src[rr][cc].equals("#"))) {
                                        burning = true;
                                    }
                                }
                            }
                            dc = dc + 1;
                        }
                        dr = dr + 1;
                    }
                    if (burning || ((Boolean)(chance(f)))) {
                        next = "#";
                    }
                } else                 if (((Boolean)(chance(p)))) {
                    next = "T";
                }
                row_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(next)).toArray(String[]::new)));
                c_1 = c_1 + 1;
            }
            dst = ((String[][])(appendObj(dst, row_1)));
            r_1 = r_1 + 1;
        }
        return dst;
    }

    static void printBoard(String[][] b) {
        System.out.println((String)(_repeat("__", cols)) + "\n\n");
        int r_2 = 0;
        while (r_2 < rows) {
            String line = "";
            int c_2 = 0;
            while (c_2 < cols) {
                String cell_1 = b[r_2][c_2];
                if ((cell_1.equals(" "))) {
                    line = line + "  ";
                } else {
                    line = line + " " + cell_1;
                }
                c_2 = c_2 + 1;
            }
            System.out.println(line + "\n");
            r_2 = r_2 + 1;
        }
    }
    public static void main(String[] args) {
        rows = 20;
        cols = 30;
        p = 0.01;
        f = 0.001;
        board = ((String[][])(newBoard()));
        printBoard(((String[][])(board)));
        board = ((String[][])(step(((String[][])(board)))));
        printBoard(((String[][])(board)));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }
}
