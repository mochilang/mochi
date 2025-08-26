public class Main {

    static double to_float(java.math.BigInteger x) {
        return (double)(((java.math.BigInteger)(x)).doubleValue() * (double)(1.0));
    }

    static double round6(double x) {
        double factor = (double)(1000000.0);
        return (double)((double)(((Number)(((Number)((double)((double)(x) * (double)(factor)) + (double)(0.5))).intValue())).doubleValue()) / (double)(factor));
    }

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return (double)(guess);
    }

    static boolean validate(double[] values) {
        if (new java.math.BigInteger(String.valueOf(values.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return false;
        }
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(values.length))) < 0) {
            if ((double)(values[_idx((values).length, ((java.math.BigInteger)(i_3)).longValue())]) <= (double)(0.0)) {
                return false;
            }
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        return true;
    }

    static double effusion_ratio(double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{(double)(m1), (double)(m2)})))) {
            System.out.println("ValueError: Molar mass values must greater than 0.");
            return (double)(0.0);
        }
        return (double)(round6((double)(sqrtApprox((double)((double)(m2) / (double)(m1))))));
    }

    static double first_effusion_rate(double rate, double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{(double)(rate), (double)(m1), (double)(m2)})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return (double)(0.0);
        }
        return (double)(round6((double)((double)(rate) * (double)(sqrtApprox((double)((double)(m2) / (double)(m1)))))));
    }

    static double second_effusion_rate(double rate, double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{(double)(rate), (double)(m1), (double)(m2)})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return (double)(0.0);
        }
        return (double)(round6((double)((double)(rate) / (double)(sqrtApprox((double)((double)(m2) / (double)(m1)))))));
    }

    static double first_molar_mass(double mass, double r1, double r2) {
        if (!(Boolean)validate(((double[])(new double[]{(double)(mass), (double)(r1), (double)(r2)})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return (double)(0.0);
        }
        double ratio_1 = (double)((double)(r1) / (double)(r2));
        return (double)(round6((double)((double)(mass) / (double)(((double)(ratio_1) * (double)(ratio_1))))));
    }

    static double second_molar_mass(double mass, double r1, double r2) {
        if (!(Boolean)validate(((double[])(new double[]{(double)(mass), (double)(r1), (double)(r2)})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return (double)(0.0);
        }
        double ratio_3 = (double)((double)(r1) / (double)(r2));
        return (double)(round6((double)((double)(((double)(ratio_3) * (double)(ratio_3))) / (double)(mass))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(effusion_ratio((double)(2.016), (double)(4.002)));
            System.out.println(first_effusion_rate((double)(1.0), (double)(2.016), (double)(4.002)));
            System.out.println(second_effusion_rate((double)(1.0), (double)(2.016), (double)(4.002)));
            System.out.println(first_molar_mass((double)(2.0), (double)(1.408943), (double)(0.709752)));
            System.out.println(second_molar_mass((double)(2.0), (double)(1.408943), (double)(0.709752)));
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

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
