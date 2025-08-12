public class Main {
    static long height;

    static void move_tower(long height, String from_pole, String to_pole, String with_pole) {
        if (height >= (long)(1)) {
            move_tower((long)(height - (long)(1)), from_pole, with_pole, to_pole);
            move_disk(from_pole, to_pole);
            move_tower((long)(height - (long)(1)), with_pole, to_pole, from_pole);
        }
    }

    static void move_disk(String fp, String tp) {
        System.out.println("moving disk from " + fp + " to " + tp);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            height = 3L;
            move_tower(height, "A", "B", "C");
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
