public class Main {
    static class Window {
        int x;
        int y;
        int w;
        int h;
        boolean maximized;
        boolean iconified;
        boolean visible;
        boolean shifted;
        Window(int x, int y, int w, int h, boolean maximized, boolean iconified, boolean visible, boolean shifted) {
            this.x = x;
            this.y = y;
            this.w = w;
            this.h = h;
            this.maximized = maximized;
            this.iconified = iconified;
            this.visible = visible;
            this.shifted = shifted;
        }
        Window() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'w': %s, 'h': %s, 'maximized': %s, 'iconified': %s, 'visible': %s, 'shifted': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(w), String.valueOf(h), String.valueOf(maximized), String.valueOf(iconified), String.valueOf(visible), String.valueOf(shifted));
        }
    }


    static void showState(Window w, String label) {
        System.out.println(label + ": pos=(" + _p(w.x) + "," + _p(w.y) + ") size=(" + _p(w.w) + "x" + _p(w.h) + ") max=" + _p(w.maximized) + " icon=" + _p(w.iconified) + " visible=" + _p(w.visible));
    }

    static Window maximize(Window w) {
w.maximized = true;
w.w = 800;
w.h = 600;
        return w;
    }

    static Window unmaximize(Window w) {
w.maximized = false;
w.w = 640;
w.h = 480;
        return w;
    }

    static Window iconify(Window w) {
w.iconified = true;
w.visible = false;
        return w;
    }

    static Window deiconify(Window w) {
w.iconified = false;
w.visible = true;
        return w;
    }

    static Window hide(Window w) {
w.visible = false;
        return w;
    }

    static Window showWindow(Window w) {
w.visible = true;
        return w;
    }

    static Window move(Window w) {
        if (w.shifted) {
w.x = w.x - 10;
w.y = w.y - 10;
        } else {
w.x = w.x + 10;
w.y = w.y + 10;
        }
w.shifted = !w.shifted;
        return w;
    }

    static void main() {
        Window win = new Window(100, 100, 640, 480, false, false, true, false);
        showState(win, "Start");
        win = maximize(win);
        showState(win, "Maximize");
        win = unmaximize(win);
        showState(win, "Unmaximize");
        win = iconify(win);
        showState(win, "Iconify");
        win = deiconify(win);
        showState(win, "Deiconify");
        win = hide(win);
        showState(win, "Hide");
        win = showWindow(win);
        showState(win, "Show");
        win = move(win);
        showState(win, "Move");
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
