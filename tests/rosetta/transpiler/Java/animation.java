public class Main {
    static String msg = "Hello World! ";
    static int shift = 0;
    static int inc = 1;
    static int clicks = 0;
    static int frames = 0;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (clicks < 5) {
                String line = "";
                int i = 0;
                while (i < msg.length()) {
                    int idx = (shift + i) % msg.length();
                    line = String.valueOf(line + msg.substring(idx, idx + 1));
                    i = i + 1;
                }
                System.out.println(line);
                shift = (shift + inc) % msg.length();
                frames = frames + 1;
                if (frames % msg.length() == 0) {
                    inc = msg.length() - inc;
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
