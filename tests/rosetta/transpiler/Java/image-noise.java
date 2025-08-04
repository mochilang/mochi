public class Main {
    static int width;
    static int height;
    static int seed;
    static int y;

    static int nextRand() {
        seed = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
        return seed;
    }

    static int randBit() {
        int r = nextRand();
        if (Math.floorMod(r, 2) == 0) {
            return 0;
        }
        return 255;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            width = 320;
            height = 240;
            seed = Math.floorMod(_now(), 2147483647);
            System.out.println("P2");
            System.out.println(_p(width) + " " + _p(height));
            System.out.println("255");
            y = 0;
            while (y < height) {
                String line = "";
                int x = 0;
                while (x < width) {
                    int val = randBit();
                    line = line + _p(val);
                    if (x < width - 1) {
                        line = line + " ";
                    }
                    x = x + 1;
                }
                System.out.println(line);
                y = y + 1;
            }
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
