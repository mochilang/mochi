public class Main {
    static String msg;
    static int shift;
    static int inc;
    static int clicks;
    static int frames;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            msg = "Hello World! ";
            shift = 0;
            inc = 1;
            clicks = 0;
            frames = 0;
            while (clicks < 5) {
                String line = "";
                int i = 0;
                while (i < _runeLen(msg)) {
                    int idx = Math.floorMod((shift + i), _runeLen(msg));
                    line = line + msg.substring(idx, idx + 1);
                    i = i + 1;
                }
                System.out.println(line);
                shift = Math.floorMod((shift + inc), _runeLen(msg));
                frames = frames + 1;
                if (Math.floorMod(frames, _runeLen(msg)) == 0) {
                    inc = _runeLen(msg) - inc;
                    clicks = clicks + 1;
                }
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
