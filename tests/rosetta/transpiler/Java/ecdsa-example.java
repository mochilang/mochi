public class Main {
    static Object res = testpkg.ECDSAExample();

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("Private key:\nD: " + (String)(res.D));
            System.out.println("\nPublic key:");
            System.out.println("X: " + (String)(res.X));
            System.out.println("Y: " + (String)(res.Y));
            System.out.println("\nMessage: Rosetta Code");
            System.out.println("Hash   : " + (String)(res.Hash));
            System.out.println("\nSignature:");
            System.out.println("R: " + (String)(res.R));
            System.out.println("S: " + (String)(res.S));
            System.out.println("\nSignature verified: " + String.valueOf(res.Valid));
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
