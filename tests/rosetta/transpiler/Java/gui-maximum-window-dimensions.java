public class Main {
    static class Screen {
        int w;
        int h;
        Screen(int w, int h) {
            this.w = w;
            this.h = h;
        }
        @Override public String toString() {
            return String.format("{'w': %s, 'h': %s}", String.valueOf(w), String.valueOf(h));
        }
    }

    static class Window {
        int x;
        int y;
        int w;
        int h;
        boolean maximized;
        Window(int x, int y, int w, int h, boolean maximized) {
            this.x = x;
            this.y = y;
            this.w = w;
            this.h = h;
            this.maximized = maximized;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'w': %s, 'h': %s, 'maximized': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(w), String.valueOf(h), String.valueOf(maximized));
        }
    }


    static Window maximize(Screen s, Window win) {
win.w = s.w;
win.h = s.h;
win.maximized = true;
        return win;
    }

    static void main() {
        Screen screen = new Screen(1920, 1080);
        System.out.println("Screen size: " + _p(screen.w) + " x " + _p(screen.h));
        Window win = new Window(50, 50, 800, 600, false);
        win = maximize(screen, win);
        System.out.println("Max usable : " + _p(win.w) + " x " + _p(win.h));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
