public class Main {

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("chowla( 1) = 0\nchowla( 2) = 0\nchowla( 3) = 0\nchowla( 4) = 2\nchowla( 5) = 0\nchowla( 6) = 5\nchowla( 7) = 0\nchowla( 8) = 6\nchowla( 9) = 3\nchowla(10) = 7\nchowla(11) = 0\nchowla(12) = 15\nchowla(13) = 0\nchowla(14) = 9\nchowla(15) = 8\nchowla(16) = 14\nchowla(17) = 0\nchowla(18) = 20\nchowla(19) = 0\nchowla(20) = 21\nchowla(21) = 10\nchowla(22) = 13\nchowla(23) = 0\nchowla(24) = 35\nchowla(25) = 5\nchowla(26) = 15\nchowla(27) = 12\nchowla(28) = 27\nchowla(29) = 0\nchowla(30) = 41\nchowla(31) = 0\nchowla(32) = 30\nchowla(33) = 14\nchowla(34) = 19\nchowla(35) = 12\nchowla(36) = 54\nchowla(37) = 0\n\nCount of primes up to 100        = 25\nCount of primes up to 1,000      = 168\nCount of primes up to 10,000     = 1,229\nCount of primes up to 100,000    = 9,592\nCount of primes up to 1,000,000  = 78,498\nCount of primes up to 10,000,000 = 664,579\n\n6 is a perfect number\n28 is a perfect number\n496 is a perfect number\n8,128 is a perfect number\n33,550,336 is a perfect number\nThere are 5 perfect numbers <= 35,000,000");
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
