public class Main {

    static int indexOf(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static double floorf(double x) {
        int y = ((Number)(x)).intValue();
        return ((Number)(y)).doubleValue();
    }

    static double powf(double base, int exp) {
        double r = 1.0;
        int i_1 = 0;
        while (i_1 < exp) {
            r = r * base;
            i_1 = i_1 + 1;
        }
        return r;
    }

    static String fmtF(double x, int width, int prec) {
        double factor = powf(10.0, prec);
        double y_1 = floorf(x * factor + 0.5) / factor;
        String s = (String)(_p(y_1));
        int dot = ((Number)(s.indexOf("."))).intValue();
        if (dot == 0 - 1) {
            s = s + ".";
            int j = 0;
            while (j < prec) {
                s = s + "0";
                j = j + 1;
            }
        } else {
            int decs = _runeLen(s) - dot - 1;
            while (decs < prec) {
                s = s + "0";
                decs = decs + 1;
            }
        }
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static double expf(double x) {
        if (x < 0.0) {
            return 1.0 / expf(-x);
        }
        double term = 1.0;
        double sum = 1.0;
        int i_2 = 1;
        while (i_2 < 20) {
            term = term * x / (((Number)(i_2)).doubleValue());
            sum = sum + term;
            i_2 = i_2 + 1;
        }
        return sum;
    }

    static double eulerStep(java.util.function.BiFunction<Double,Double,Double> f, double x, double y, double h) {
        return y + h * f.apply(x, y);
    }

    static java.util.function.Function<Double,Double> newCoolingRate(double k) {
        return (dt) -> -k * dt;
    }

    static java.util.function.Function<Double,Double> newTempFunc(double k, double ambient, double initial) {
        return (t) -> ambient + (initial - ambient) * expf(-k * t);
    }

    static java.util.function.BiFunction<Double,Double,Double> newCoolingRateDy(double k, double ambient) {
        java.util.function.Function<Double,Double> cr = newCoolingRate(k);
        return (_x, obj) -> cr.apply(obj - ambient);
    }

    static void main() {
        double k = 0.07;
        double tempRoom = 20.0;
        double tempObject = 100.0;
        java.util.function.BiFunction<Double,Double,Double> fcr = newCoolingRateDy(k, tempRoom);
        java.util.function.Function<Double,Double> analytic = newTempFunc(k, tempRoom, tempObject);
        for (double step : new double[]{2.0, 5.0, 10.0}) {
            System.out.println("Step size = " + String.valueOf(fmtF(step, 0, 1)));
            System.out.println(" Time Euler's Analytic");
            double temp = tempObject;
            double time = 0.0;
            while (time <= 100.0) {
                String line = String.valueOf(fmtF(time, 5, 1)) + " " + String.valueOf(fmtF(temp, 7, 3)) + " " + String.valueOf(fmtF(analytic.apply(time), 7, 3));
                System.out.println(line);
                temp = eulerStep(fcr, time, temp, step);
                time = time + step;
            }
            System.out.println("");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
