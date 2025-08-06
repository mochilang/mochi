public class Main {
    static int rank_of_matrix(double[][] matrix) {
        int rows = matrix.length;
        if (rows == 0) {
            return 0;
        }
        int columns = matrix[0].length > 0 ? matrix[0].length : 0;
        int rank = rows < columns ? rows : columns;
        int row = 0;
        while (row < rank) {
            if (matrix[row][row] != 0.0) {
                int col = row + 1;
                while (col < rows) {
                    double mult = matrix[col][row] / matrix[row][row];
                    int i = row;
                    while (i < columns) {
matrix[col][i] = matrix[col][i] - mult * matrix[row][i];
                        i = i + 1;
                    }
                    col = col + 1;
                }
            } else {
                boolean reduce = true;
                int i_1 = row + 1;
                while (i_1 < rows) {
                    if (matrix[i_1][row] != 0.0) {
                        double[] temp = ((double[])(matrix[row]));
matrix[row] = ((double[])(matrix[i_1]));
matrix[i_1] = ((double[])(temp));
                        reduce = false;
                        break;
                    }
                    i_1 = i_1 + 1;
                }
                if (reduce) {
                    rank = rank - 1;
                    int j = 0;
                    while (j < rows) {
matrix[j][row] = matrix[j][rank];
                        j = j + 1;
                    }
                }
                row = row - 1;
            }
            row = row + 1;
        }
        return rank;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
