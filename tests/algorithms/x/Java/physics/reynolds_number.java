public class Main {

    static double fabs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        } else {
            return x;
        }
    }

    static double reynolds_number(double density, double velocity, double diameter, double viscosity) {
        if ((double)(density) <= (double)(0.0) || (double)(diameter) <= (double)(0.0) || (double)(viscosity) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("please ensure that density, diameter and viscosity are positive"));
        }
        return (double)(((double)((double)(density) * (double)(fabs((double)(velocity)))) * (double)(diameter))) / (double)(viscosity);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(reynolds_number((double)(900.0), (double)(2.5), (double)(0.05), (double)(0.4)));
            System.out.println(reynolds_number((double)(450.0), (double)(3.86), (double)(0.078), (double)(0.23)));
            System.out.println(reynolds_number((double)(234.0), (double)(-4.5), (double)(0.3), (double)(0.44)));
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
