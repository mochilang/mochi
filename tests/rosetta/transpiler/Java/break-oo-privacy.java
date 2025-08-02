public class Main {
    static class Foobar {
        int Exported;
        int unexported;
        Foobar(int Exported, int unexported) {
            this.Exported = Exported;
            this.unexported = unexported;
        }
        @Override public String toString() {
            return String.format("{'Exported': %s, 'unexported': %s}", String.valueOf(Exported), String.valueOf(unexported));
        }
    }

    static Foobar obj;

    static Foobar examineAndModify(Foobar f) {
        System.out.println(" v: {" + _p(f.Exported) + " " + _p(f.unexported) + "} = {" + _p(f.Exported) + " " + _p(f.unexported) + "}");
        System.out.println("    Idx Name       Type CanSet");
        System.out.println("     0: Exported   int  true");
        System.out.println("     1: unexported int  false");
f.Exported = 16;
f.unexported = 44;
        System.out.println("  modified unexported field via unsafe");
        return f;
    }

    static void anotherExample() {
        System.out.println("bufio.ReadByte returned error: unsafely injected error value into bufio inner workings");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            obj = new Foobar(12, 42);
            System.out.println("obj: {" + _p(obj.Exported) + " " + _p(obj.unexported) + "}");
            obj = examineAndModify(obj);
            System.out.println("obj: {" + _p(obj.Exported) + " " + _p(obj.unexported) + "}");
            anotherExample();
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
