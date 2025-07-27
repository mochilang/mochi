public class Main {
    static class TinyInt {
        int value;
        TinyInt(int value) {
            this.value = value;
        }
        TinyInt Add(TinyInt t2) {
            return NewTinyInt(((Number)(value)).intValue() + t2.value);
        }
        TinyInt Sub(TinyInt t2) {
            return NewTinyInt(((Number)(value)).intValue() - t2.value);
        }
        TinyInt Mul(TinyInt t2) {
            return NewTinyInt(((Number)(value)).intValue() * t2.value);
        }
        TinyInt Div(TinyInt t2) {
            return NewTinyInt(((Number)(value)).intValue() / t2.value);
        }
        TinyInt Rem(TinyInt t2) {
            return NewTinyInt(Math.floorMod(value, t2.value));
        }
        TinyInt Inc() {
            return Add(NewTinyInt(1));
        }
        TinyInt Dec() {
            return Sub(NewTinyInt(1));
        }
        @Override public String toString() {
            return String.format("{'value': %s}", String.valueOf(value));
        }
    }


    static TinyInt NewTinyInt(int i) {
        if (i < 1) {
            i = 1;
        } else         if (i > 10) {
            i = 10;
        }
        return new TinyInt(i);
    }

    static void main() {
        TinyInt t1 = NewTinyInt(6);
        TinyInt t2 = NewTinyInt(3);
        System.out.println("t1      = " + String.valueOf(t1.value));
        System.out.println("t2      = " + String.valueOf(t2.value));
        System.out.println("t1 + t2 = " + String.valueOf(t1.Add(t2).value));
        System.out.println("t1 - t2 = " + String.valueOf(t1.Sub(t2).value));
        System.out.println("t1 * t2 = " + String.valueOf(t1.Mul(t2).value));
        System.out.println("t1 / t2 = " + String.valueOf(t1.Div(t2).value));
        System.out.println("t1 % t2 = " + String.valueOf(t1.Rem(t2).value));
        System.out.println("t1 + 1  = " + String.valueOf(t1.Inc().value));
        System.out.println("t1 - 1  = " + String.valueOf(t1.Dec().value));
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
}
