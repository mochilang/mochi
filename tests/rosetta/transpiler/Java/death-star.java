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

    static class Sphere {
        double cx;
        double cy;
        double cz;
        double r;
        Sphere(double cx, double cy, double cz, double r) {
            this.cx = cx;
            this.cy = cy;
            this.cz = cz;
            this.r = r;
        }
        @Override public String toString() {
            return String.format("{'cx': %s, 'cy': %s, 'cz': %s, 'r': %s}", String.valueOf(cx), String.valueOf(cy), String.valueOf(cz), String.valueOf(r));
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

    static java.util.Map<String,Object> hitSphere(Sphere s, double x, double y) {
        double dx = x - s.cx;
        double dy = y - s.cy;
        double zsq = s.r * s.r - (dx * dx + dy * dy);
        if (zsq < 0.0) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("hit", false)));
        }
        double z = sqrtApprox(zsq);
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("hit", true), java.util.Map.entry("z1", s.cz - z), java.util.Map.entry("z2", s.cz + z)));
    }

    static void main() {
        String shades = ".:!*oe&#%@";
        V3 light = normalize(new V3((-50.0), 30.0, 50.0));
        Sphere pos = new Sphere(20.0, 20.0, 0.0, 20.0);
        Sphere neg = new Sphere(1.0, 1.0, (-6.0), 20.0);
        int yi = 0;
        while (yi <= 40) {
            double y = (((Number)(yi)).doubleValue()) + 0.5;
            String line = "";
            int xi = -20;
            while (xi <= 60) {
                double x = ((((Number)(xi)).doubleValue()) - pos.cx) / 2.0 + 0.5 + pos.cx;
                java.util.Map<String,Object> hb = hitSphere(pos, x, y);
                if (!((boolean) (hb.get("hit")))) {
                    line = line + " ";
                    xi = xi + 1;
                    continue;
                }
                double zb1 = (double)(((double) (hb.get("z1"))));
                double zb2 = (double)(((double) (hb.get("z2"))));
                java.util.Map<String,Object> hs = hitSphere(neg, x, y);
                int hitRes = 1;
                if (!((boolean) (hs.get("hit")))) {
                    hitRes = 1;
                } else                 if ((double)(((double) (hs.get("z1")))) > zb1) {
                    hitRes = 1;
                } else                 if ((double)(((double) (hs.get("z2")))) > zb2) {
                    hitRes = 0;
                } else                 if ((double)(((double) (hs.get("z2")))) > zb1) {
                    hitRes = 2;
                } else {
                    hitRes = 1;
                }
                if (hitRes == 0) {
                    line = line + " ";
                    xi = xi + 1;
                    continue;
                }
                V3 vec;
                if (hitRes == 1) {
                    vec = new V3(x - pos.cx, y - pos.cy, zb1 - pos.cz);
                } else {
                    vec = new V3(neg.cx - x, neg.cy - y, neg.cz - (double)(((double) (hs.get("z2")))));
                }
                vec = normalize(vec);
                double b = powf(dot(light, vec), 2) + 0.5;
                int intensity = ((Number)(((1.0 - b) * (((Number)(_runeLen(shades))).doubleValue())))).intValue();
                if (intensity < 0) {
                    intensity = 0;
                }
                if (intensity >= _runeLen(shades)) {
                    intensity = _runeLen(shades) - 1;
                }
                line = line + _substr(shades, intensity, intensity + 1);
                xi = xi + 1;
            }
            System.out.println(line);
            yi = yi + 1;
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
}
