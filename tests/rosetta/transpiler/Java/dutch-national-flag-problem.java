public class Main {

    static String listStr(int[] xs) {
        String s = "[";
        int i = 0;
        while (i < xs.length) {
            s = s + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static boolean ordered(int[] xs) {
        if (xs.length == 0) {
            return true;
        }
        int prev = xs[0];
        int i = 1;
        while (i < xs.length) {
            if (xs[i] < prev) {
                return false;
            }
            prev = xs[i];
            i = i + 1;
        }
        return true;
    }

    static int[] outOfOrder(int n) {
        if (n < 2) {
            return new int[]{};
        }
        int[] r = new int[]{};
        while (true) {
            r = new int[]{};
            int i = 0;
            while (i < n) {
                r = java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(Math.floorMod(_now(), 3))).toArray();
                i = i + 1;
            }
            if (!(Boolean)ordered(r)) {
                break;
            }
        }
        return r;
    }

    static int[] sort3(int[] a) {
        int lo = 0;
        int mid = 0;
        int hi = a.length - 1;
        while (mid <= hi) {
            int v = a[mid];
            if (v == 0) {
                int tmp = a[lo];
a[lo] = a[mid];
a[mid] = tmp;
                lo = lo + 1;
                mid = mid + 1;
            } else             if (v == 1) {
                mid = mid + 1;
            } else {
                int tmp = a[mid];
a[mid] = a[hi];
a[hi] = tmp;
                hi = hi - 1;
            }
        }
        return a;
    }

    static void main() {
        int[] f = outOfOrder(12);
        System.out.println(listStr(f));
        f = sort3(f);
        System.out.println(listStr(f));
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
