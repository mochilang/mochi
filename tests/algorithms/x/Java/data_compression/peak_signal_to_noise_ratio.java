public class Main {
    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return guess_1;
    }

    static double ln(double x) {
        double t = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double term_1 = (double)(t);
        double sum_1 = (double)(0.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(19)) <= 0) {
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)((((Number)(n_1)).doubleValue()))));
            term_1 = (double)((double)((double)(term_1) * (double)(t)) * (double)(t));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(2))));
        }
        return (double)(2.0) * (double)(sum_1);
    }

    static double log10(double x) {
        return (double)(Math.log(x)) / (double)(Math.log(10.0));
    }

    static double peak_signal_to_noise_ratio(java.math.BigInteger[][] original, java.math.BigInteger[][] contrast) {
        double mse = (double)(0.0);
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(original.length))) < 0) {
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(original[(int)(((java.math.BigInteger)(i_3)).longValue())].length))) < 0) {
                double diff_1 = (double)(((Number)((original[(int)(((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_1)).longValue())].subtract(contrast[(int)(((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_1)).longValue())])))).doubleValue());
                mse = (double)((double)(mse) + (double)((double)(diff_1) * (double)(diff_1)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        double size_1 = (double)(((Number)((new java.math.BigInteger(String.valueOf(original.length)).multiply(new java.math.BigInteger(String.valueOf(original[(int)(0L)].length)))))).doubleValue());
        mse = (double)((double)(mse) / (double)(size_1));
        if ((double)(mse) == (double)(0.0)) {
            return 100.0;
        }
        double PIXEL_MAX_1 = (double)(255.0);
        return (double)(20.0) * (double)(log10((double)((double)(PIXEL_MAX_1) / (double)(sqrtApprox((double)(mse))))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
