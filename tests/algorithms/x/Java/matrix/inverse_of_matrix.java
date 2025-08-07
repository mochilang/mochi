public class Main {
    static double[][] m2 = new double[0][];
    static double[][] m3 = new double[0][];

    static double[][] inverse_of_matrix(double[][] matrix) {
        if (matrix.length == 2 && matrix[0].length == 2 && matrix[1].length == 2) {
            double det = matrix[0][0] * matrix[1][1] - matrix[1][0] * matrix[0][1];
            if (det == 0.0) {
                System.out.println("This matrix has no inverse.");
                return new double[][]{};
            }
            return new double[][]{new double[]{matrix[1][1] / det, -matrix[0][1] / det}, new double[]{-matrix[1][0] / det, matrix[0][0] / det}};
        } else         if (matrix.length == 3 && matrix[0].length == 3 && matrix[1].length == 3 && matrix[2].length == 3) {
            double det_1 = matrix[0][0] * matrix[1][1] * matrix[2][2] + matrix[0][1] * matrix[1][2] * matrix[2][0] + matrix[0][2] * matrix[1][0] * matrix[2][1] - (matrix[0][2] * matrix[1][1] * matrix[2][0] + matrix[0][1] * matrix[1][0] * matrix[2][2] + matrix[0][0] * matrix[1][2] * matrix[2][1]);
            if (det_1 == 0.0) {
                System.out.println("This matrix has no inverse.");
                return new double[][]{};
            }
            double[][] cof = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}}));
cof[0][0] = matrix[1][1] * matrix[2][2] - matrix[1][2] * matrix[2][1];
cof[0][1] = -(matrix[1][0] * matrix[2][2] - matrix[1][2] * matrix[2][0]);
cof[0][2] = matrix[1][0] * matrix[2][1] - matrix[1][1] * matrix[2][0];
cof[1][0] = -(matrix[0][1] * matrix[2][2] - matrix[0][2] * matrix[2][1]);
cof[1][1] = matrix[0][0] * matrix[2][2] - matrix[0][2] * matrix[2][0];
cof[1][2] = -(matrix[0][0] * matrix[2][1] - matrix[0][1] * matrix[2][0]);
cof[2][0] = matrix[0][1] * matrix[1][2] - matrix[0][2] * matrix[1][1];
cof[2][1] = -(matrix[0][0] * matrix[1][2] - matrix[0][2] * matrix[1][0]);
cof[2][2] = matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
            double[][] inv = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}}));
            int i = 0;
            while (i < 3) {
                int j = 0;
                while (j < 3) {
inv[i][j] = cof[j][i] / det_1;
                    j = j + 1;
                }
                i = i + 1;
            }
            return inv;
        }
        System.out.println("Please provide a matrix of size 2x2 or 3x3.");
        return new double[][]{};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            m2 = ((double[][])(new double[][]{new double[]{2.0, 5.0}, new double[]{2.0, 0.0}}));
            System.out.println(inverse_of_matrix(((double[][])(m2))));
            m3 = ((double[][])(new double[][]{new double[]{2.0, 5.0, 7.0}, new double[]{2.0, 0.0, 1.0}, new double[]{1.0, 2.0, 3.0}}));
            System.out.println(inverse_of_matrix(((double[][])(m3))));
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
