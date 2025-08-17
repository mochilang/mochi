public class Main {
    static double[][] m2 = ((double[][])(new double[][]{new double[]{2.0, 5.0}, new double[]{2.0, 0.0}}));
    static double[][] m3 = ((double[][])(new double[][]{new double[]{2.0, 5.0, 7.0}, new double[]{2.0, 0.0, 1.0}, new double[]{1.0, 2.0, 3.0}}));

    static double[][] inverse_of_matrix(double[][] matrix) {
        if ((long)(matrix.length) == 2L && (long)(matrix[(int)(0L)].length) == 2L && (long)(matrix[(int)(1L)].length) == 2L) {
            double det = (double)((double)((double)(matrix[(int)(0L)][(int)(0L)]) * (double)(matrix[(int)(1L)][(int)(1L)])) - (double)((double)(matrix[(int)(1L)][(int)(0L)]) * (double)(matrix[(int)(0L)][(int)(1L)])));
            if ((double)(det) == (double)(0.0)) {
                System.out.println("This matrix has no inverse.");
                return new double[][]{};
            }
            return new double[][]{new double[]{(double)(matrix[(int)(1L)][(int)(1L)]) / (double)(det), (double)(-matrix[(int)(0L)][(int)(1L)]) / (double)(det)}, new double[]{(double)(-matrix[(int)(1L)][(int)(0L)]) / (double)(det), (double)(matrix[(int)(0L)][(int)(0L)]) / (double)(det)}};
        } else         if ((long)(matrix.length) == 3L && (long)(matrix[(int)(0L)].length) == 3L && (long)(matrix[(int)(1L)].length) == 3L && (long)(matrix[(int)(2L)].length) == 3L) {
            double det_1 = (double)((double)((double)((double)((double)((double)(matrix[(int)(0L)][(int)(0L)]) * (double)(matrix[(int)(1L)][(int)(1L)])) * (double)(matrix[(int)(2L)][(int)(2L)])) + (double)((double)((double)(matrix[(int)(0L)][(int)(1L)]) * (double)(matrix[(int)(1L)][(int)(2L)])) * (double)(matrix[(int)(2L)][(int)(0L)]))) + (double)((double)((double)(matrix[(int)(0L)][(int)(2L)]) * (double)(matrix[(int)(1L)][(int)(0L)])) * (double)(matrix[(int)(2L)][(int)(1L)]))) - (double)(((double)((double)((double)((double)(matrix[(int)(0L)][(int)(2L)]) * (double)(matrix[(int)(1L)][(int)(1L)])) * (double)(matrix[(int)(2L)][(int)(0L)])) + (double)((double)((double)(matrix[(int)(0L)][(int)(1L)]) * (double)(matrix[(int)(1L)][(int)(0L)])) * (double)(matrix[(int)(2L)][(int)(2L)]))) + (double)((double)((double)(matrix[(int)(0L)][(int)(0L)]) * (double)(matrix[(int)(1L)][(int)(2L)])) * (double)(matrix[(int)(2L)][(int)(1L)])))));
            if ((double)(det_1) == (double)(0.0)) {
                System.out.println("This matrix has no inverse.");
                return new double[][]{};
            }
            double[][] cof = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}}));
cof[(int)(0L)][(int)(0L)] = (double)((double)((double)(matrix[(int)(1L)][(int)(1L)]) * (double)(matrix[(int)(2L)][(int)(2L)])) - (double)((double)(matrix[(int)(1L)][(int)(2L)]) * (double)(matrix[(int)(2L)][(int)(1L)])));
cof[(int)(0L)][(int)(1L)] = (double)(-((double)((double)(matrix[(int)(1L)][(int)(0L)]) * (double)(matrix[(int)(2L)][(int)(2L)])) - (double)((double)(matrix[(int)(1L)][(int)(2L)]) * (double)(matrix[(int)(2L)][(int)(0L)]))));
cof[(int)(0L)][(int)(2L)] = (double)((double)((double)(matrix[(int)(1L)][(int)(0L)]) * (double)(matrix[(int)(2L)][(int)(1L)])) - (double)((double)(matrix[(int)(1L)][(int)(1L)]) * (double)(matrix[(int)(2L)][(int)(0L)])));
cof[(int)(1L)][(int)(0L)] = (double)(-((double)((double)(matrix[(int)(0L)][(int)(1L)]) * (double)(matrix[(int)(2L)][(int)(2L)])) - (double)((double)(matrix[(int)(0L)][(int)(2L)]) * (double)(matrix[(int)(2L)][(int)(1L)]))));
cof[(int)(1L)][(int)(1L)] = (double)((double)((double)(matrix[(int)(0L)][(int)(0L)]) * (double)(matrix[(int)(2L)][(int)(2L)])) - (double)((double)(matrix[(int)(0L)][(int)(2L)]) * (double)(matrix[(int)(2L)][(int)(0L)])));
cof[(int)(1L)][(int)(2L)] = (double)(-((double)((double)(matrix[(int)(0L)][(int)(0L)]) * (double)(matrix[(int)(2L)][(int)(1L)])) - (double)((double)(matrix[(int)(0L)][(int)(1L)]) * (double)(matrix[(int)(2L)][(int)(0L)]))));
cof[(int)(2L)][(int)(0L)] = (double)((double)((double)(matrix[(int)(0L)][(int)(1L)]) * (double)(matrix[(int)(1L)][(int)(2L)])) - (double)((double)(matrix[(int)(0L)][(int)(2L)]) * (double)(matrix[(int)(1L)][(int)(1L)])));
cof[(int)(2L)][(int)(1L)] = (double)(-((double)((double)(matrix[(int)(0L)][(int)(0L)]) * (double)(matrix[(int)(1L)][(int)(2L)])) - (double)((double)(matrix[(int)(0L)][(int)(2L)]) * (double)(matrix[(int)(1L)][(int)(0L)]))));
cof[(int)(2L)][(int)(2L)] = (double)((double)((double)(matrix[(int)(0L)][(int)(0L)]) * (double)(matrix[(int)(1L)][(int)(1L)])) - (double)((double)(matrix[(int)(0L)][(int)(1L)]) * (double)(matrix[(int)(1L)][(int)(0L)])));
            double[][] inv = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}}));
            long i = 0L;
            while ((long)(i) < 3L) {
                long j = 0L;
                while ((long)(j) < 3L) {
inv[(int)((long)(i))][(int)((long)(j))] = (double)((double)(cof[(int)((long)(j))][(int)((long)(i))]) / (double)(det_1));
                    j = (long)((long)(j) + 1L);
                }
                i = (long)((long)(i) + 1L);
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
            System.out.println(inverse_of_matrix(((double[][])(m2))));
            System.out.println(inverse_of_matrix(((double[][])(m3))));
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
}
