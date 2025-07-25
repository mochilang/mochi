public class Main {
    static boolean[] doors = new boolean[]{};

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (int i = 0; i < 100; i++) {
                doors = appendBool(doors, false);
            }
            for (int pass = 1; pass < 101; pass++) {
                int idx = pass - 1;
                while (idx < 100) {
doors[idx] = !doors[idx];
                    idx = idx + pass;
                }
            }
            for (int row = 0; row < 10; row++) {
                String line = "";
                for (int col = 0; col < 10; col++) {
                    int idx = row * 10 + col;
                    if (doors[idx]) {
                        line = line + "1";
                    } else {
                        line = line + "0";
                    }
                    if (col < 9) {
                        line = line + " ";
                    }
                }
                System.out.println(line);
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
        return rt.totalMemory() - rt.freeMemory();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
