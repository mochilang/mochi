public class Main {
    static int door = 1;
    static int incrementer = 0;

    public static void main(String[] args) {
        int _benchStart = _now();
        int _benchMem = _mem();
        for (int current = 1; current < 101; current++) {
            String line = "Door " + String.valueOf(current) + " ";
            if (current == door) {
                line = line + "Open";
                incrementer = incrementer + 1;
                door = door + 2 * incrementer + 1;
            } else {
                line = line + "Closed";
            }
            System.out.println(line);
        }
        int _benchDuration = (_now() - _benchStart) / 1000;
        int _benchMemory = _mem() - _benchMem;
        System.out.println("{");
        System.out.println("  \"duration_us\": " + _benchDuration + ",");
        System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
        System.out.println("  \"name\": \"main\"");
        System.out.println("}");
        return;
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
        return (int)System.currentTimeMillis();
    }

    static int _mem() {
        Runtime rt = Runtime.getRuntime();
        return (int)(rt.totalMemory() - rt.freeMemory());
    }
}
