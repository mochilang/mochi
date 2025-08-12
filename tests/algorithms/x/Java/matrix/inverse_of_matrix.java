public class Main {
    static double[][] m2 = new double[0][];
    static double[][] m3 = new double[0][];

    static double[][] inverse_of_matrix(double[][] matrix) {
        if ((long)(matrix.length) == (long)(2) && (long)(((double[])_geto(matrix, (int)((long)(0)))).length) == (long)(2) && (long)(((double[])_geto(matrix, (int)((long)(1)))).length) == (long)(2)) {
            double det = (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1))));
            if (det == 0.0) {
                System.out.println("This matrix has no inverse.");
                return new double[][]{};
            }
            return new double[][]{new double[]{(double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1)))) / det, (double)(-_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1)))) / det}, new double[]{(double)(-_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0)))) / det, (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) / det}};
        } else         if ((long)(matrix.length) == (long)(3) && (long)(((double[])_geto(matrix, (int)((long)(0)))).length) == (long)(3) && (long)(((double[])_geto(matrix, (int)((long)(1)))).length) == (long)(3) && (long)(((double[])_geto(matrix, (int)((long)(2)))).length) == (long)(3)) {
            double det_1 = (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(2)))) + (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(0)))) + (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(1)))) - ((double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(0)))) + (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(2)))) + (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(1)))));
            if (det_1 == 0.0) {
                System.out.println("This matrix has no inverse.");
                return new double[][]{};
            }
            double[][] cof = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}}));
((double[])_geto(cof, (int)((long)(0))))[(int)((long)(0))] = (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(2)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(1))));
((double[])_geto(cof, (int)((long)(0))))[(int)((long)(1))] = -((double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(2)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(0)))));
((double[])_geto(cof, (int)((long)(0))))[(int)((long)(2))] = (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(1)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(0))));
((double[])_geto(cof, (int)((long)(1))))[(int)((long)(0))] = -((double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(2)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(1)))));
((double[])_geto(cof, (int)((long)(1))))[(int)((long)(1))] = (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(2)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(0))));
((double[])_geto(cof, (int)((long)(1))))[(int)((long)(2))] = -((double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(1)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(2)))), (int)((long)(0)))));
((double[])_geto(cof, (int)((long)(2))))[(int)((long)(0))] = (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(2)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1))));
((double[])_geto(cof, (int)((long)(2))))[(int)((long)(1))] = -((double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(2)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(2)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0)))));
((double[])_geto(cof, (int)((long)(2))))[(int)((long)(2))] = (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(0)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(1)))) - (double)(_getd(((double[])_geto(matrix, (int)((long)(0)))), (int)((long)(1)))) * (double)(_getd(((double[])_geto(matrix, (int)((long)(1)))), (int)((long)(0))));
            double[][] inv = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 0.0}}));
            long i = 0L;
            while ((long)(i) < (long)(3)) {
                long j = 0L;
                while ((long)(j) < (long)(3)) {
((double[])_geto(inv, (int)((long)(i))))[(int)((long)(j))] = (double)(_getd(((double[])_geto(cof, (int)((long)(j)))), (int)((long)(i)))) / det_1;
                    j = (long)((long)(j) + (long)(1));
                }
                i = (long)((long)(i) + (long)(1));
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

    static double _getd(double[] a, int i) {
        if (a == null) return 0.0;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0.0;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
