public class Main {

    static int absInt(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static void main() {
        int b = 1;
        while (b <= 5) {
            if (b != 5) {
                int c = 1;
                while (c <= 5) {
                    if (c != 1 && c != b) {
                        int f = 1;
                        while (f <= 5) {
                            if (f != 1 && f != 5 && f != b && f != c && absInt(f - c) > 1) {
                                int m = 1;
                                while (m <= 5) {
                                    if (m != b && m != c && m != f && m > c) {
                                        int s = 1;
                                        while (s <= 5) {
                                            if (s != b && s != c && s != f && s != m && absInt(s - f) > 1) {
                                                System.out.println("Baker in " + String.valueOf(b) + ", Cooper in " + String.valueOf(c) + ", Fletcher in " + String.valueOf(f) + ", Miller in " + String.valueOf(m) + ", Smith in " + String.valueOf(s) + ".");
                                                return;
                                            }
                                            s = s + 1;
                                        }
                                    }
                                    m = m + 1;
                                }
                            }
                            f = f + 1;
                        }
                    }
                    c = c + 1;
                }
            }
            b = b + 1;
        }
        System.out.println("No solution found.");
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
}
