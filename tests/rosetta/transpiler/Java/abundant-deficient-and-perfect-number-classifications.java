public class Main {

    static int pfacSum(int i) {
        int sum = 0;
        int p = 1;
        while (p <= i / 2) {
            if (i % p == 0) {
                sum = sum + p;
            }
            p = p + 1;
        }
        return sum;
    }

    static void main() {
        int d = 0;
        int a = 0;
        int pnum = 0;
        int i = 1;
        while (i <= 20000) {
            int j = pfacSum(i);
            if (j < i) {
                d = d + 1;
            }
            if (j == i) {
                pnum = pnum + 1;
            }
            if (j > i) {
                a = a + 1;
            }
            i = i + 1;
        }
        System.out.println("There are " + String.valueOf(d) + " deficient numbers between 1 and 20000");
        System.out.println("There are " + String.valueOf(a) + " abundant numbers  between 1 and 20000");
        System.out.println("There are " + String.valueOf(pnum) + " perfect numbers between 1 and 20000");
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
