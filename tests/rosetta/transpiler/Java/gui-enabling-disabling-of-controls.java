public class Main {
    static class State {
        boolean entry;
        boolean inc;
        boolean dec;
        State(boolean entry, boolean inc, boolean dec) {
            this.entry = entry;
            this.inc = inc;
            this.dec = dec;
        }
        @Override public String toString() {
            return String.format("{'entry': %s, 'inc': %s, 'dec': %s}", String.valueOf(entry), String.valueOf(inc), String.valueOf(dec));
        }
    }


    static State state(int v) {
        return new State(v == 0, v < 10, v > 0);
    }

    static void printState(int v) {
        State s = state(v);
        System.out.println("value=" + _p(v) + " entry=" + _p(s.entry) + " inc=" + _p(s.inc) + " dec=" + _p(s.dec));
    }

    static void main() {
        int v = 0;
        printState(v);
        while (true) {
            State s_1 = state(v);
            if (!s_1.inc) {
                break;
            }
            v = v + 1;
            printState(v);
        }
        while (true) {
            State s_2 = state(v);
            if (!s_2.dec) {
                break;
            }
            v = v - 1;
            printState(v);
        }
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
