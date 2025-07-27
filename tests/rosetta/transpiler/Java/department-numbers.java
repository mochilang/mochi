public class Main {
    static int count = 0;
    static int i = 2;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("Police  Sanitation  Fire");
            System.out.println("------  ----------  ----");
            while (i < 7) {
                int j = 1;
                while (j < 8) {
                    if (j != i) {
                        int k = 1;
                        while (k < 8) {
                            if (k != i && k != j) {
                                if (i + j + k == 12) {
                                    System.out.println("  " + String.valueOf(i) + "         " + String.valueOf(j) + "         " + String.valueOf(k));
                                    count = count + 1;
                                }
                            }
                            k = k + 1;
                        }
                    }
                    j = j + 1;
                }
                i = i + 2;
            }
            System.out.println("");
            System.out.println(String.valueOf(count) + " valid combinations");
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
