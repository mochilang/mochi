public class Main {

    static double[] sortFloats(double[] xs) {
        double[] arr = ((double[])(xs));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                if ((double)(arr[(int)(((java.math.BigInteger)(j_1)).longValue())]) > (double)(arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())])) {
                    double t_1 = (double)(arr[(int)(((java.math.BigInteger)(j_1)).longValue())]);
arr[(int)(((java.math.BigInteger)(j_1)).longValue())] = (double)(arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())]);
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = (double)(t_1);
                }
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return arr;
    }

    static double find_median_sorted_arrays(double[] nums1, double[] nums2) {
        if (new java.math.BigInteger(String.valueOf(nums1.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0 && new java.math.BigInteger(String.valueOf(nums2.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("Both input arrays are empty."));
        }
        double[] merged_1 = ((double[])(new double[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(nums1.length))) < 0) {
            merged_1 = ((double[])(appendDouble(merged_1, (double)(nums1[(int)(((java.math.BigInteger)(i_3)).longValue())]))));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
        while (j_3.compareTo(new java.math.BigInteger(String.valueOf(nums2.length))) < 0) {
            merged_1 = ((double[])(appendDouble(merged_1, (double)(nums2[(int)(((java.math.BigInteger)(j_3)).longValue())]))));
            j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
        }
        double[] sorted_1 = ((double[])(sortFloats(((double[])(merged_1)))));
        java.math.BigInteger total_1 = new java.math.BigInteger(String.valueOf(sorted_1.length));
        if (total_1.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(1)) == 0) {
            return sorted_1[(int)(((java.math.BigInteger)(total_1.divide(java.math.BigInteger.valueOf(2)))).longValue())];
        }
        double middle1_1 = (double)(sorted_1[(int)(((java.math.BigInteger)(total_1.divide(java.math.BigInteger.valueOf(2)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]);
        double middle2_1 = (double)(sorted_1[(int)(((java.math.BigInteger)(total_1.divide(java.math.BigInteger.valueOf(2)))).longValue())]);
        return (double)(((double)(middle1_1) + (double)(middle2_1))) / (double)(2.0);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(find_median_sorted_arrays(((double[])(new double[]{(double)(1.0), (double)(3.0)})), ((double[])(new double[]{(double)(2.0)}))));
            System.out.println(find_median_sorted_arrays(((double[])(new double[]{(double)(1.0), (double)(2.0)})), ((double[])(new double[]{(double)(3.0), (double)(4.0)}))));
            System.out.println(find_median_sorted_arrays(((double[])(new double[]{(double)(0.0), (double)(0.0)})), ((double[])(new double[]{(double)(0.0), (double)(0.0)}))));
            System.out.println(find_median_sorted_arrays(((double[])(new double[]{})), ((double[])(new double[]{(double)(1.0)}))));
            System.out.println(find_median_sorted_arrays(((double[])(new double[]{(double)(-1000.0)})), ((double[])(new double[]{(double)(1000.0)}))));
            System.out.println(find_median_sorted_arrays(((double[])(new double[]{(double)(-1.1), (double)(-2.2)})), ((double[])(new double[]{(double)(-3.3), (double)(-4.4)}))));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
