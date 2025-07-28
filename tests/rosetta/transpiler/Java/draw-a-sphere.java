public class Main {
    static class V3 {
        double x;
        double y;
        double z;
        V3(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'z': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(z));
        }
    }


    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double powf(double base, int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static V3 normalize(V3 v) {
        double len = sqrtApprox(v.x * v.x + v.y * v.y + v.z * v.z);
        return new V3(v.x / len, v.y / len, v.z / len);
    }

    static double dot(V3 a, V3 b) {
        double d = a.x * b.x + a.y * b.y + a.z * b.z;
        if (d < 0.0) {
            return -d;
        }
        return 0.0;
    }

    static void drawSphere(int r, int k, double ambient, V3 light, String shades) {
        int i = -r;
        while (i <= r) {
            double x = (((Number)(i)).doubleValue()) + 0.5;
            String line = "";
            int j = -(2 * r);
            while (j <= 2 * r) {
                double y = (((Number)(j)).doubleValue()) / 2.0 + 0.5;
                if (x * x + y * y <= (((Number)(r)).doubleValue()) * (((Number)(r)).doubleValue())) {
                    double zsq = (((Number)(r)).doubleValue()) * (((Number)(r)).doubleValue()) - x * x - y * y;
                    V3 vec = normalize(new V3(x, y, sqrtApprox(zsq)));
                    double b = powf(dot(light, vec), k) + ambient;
                    int intensity = ((Number)(((1.0 - b) * ((((Number)(_runeLen(shades))).doubleValue()) - 1.0)))).intValue();
                    if (intensity < 0) {
                        intensity = 0;
                    }
                    if (intensity >= _runeLen(shades)) {
                        intensity = _runeLen(shades) - 1;
                    }
                    line = line + _substr(shades, intensity, intensity + 1);
                } else {
                    line = line + " ";
                }
                j = j + 1;
            }
            System.out.println(line);
            i = i + 1;
        }
    }

    static void main() {
        String shades = ".:!*oe&#%@";
        V3 light = normalize(new V3(30.0, 30.0, -50.0));
        drawSphere(20, 4, 0.1, light, shades);
        drawSphere(10, 2, 0.4, light, shades);
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
}
