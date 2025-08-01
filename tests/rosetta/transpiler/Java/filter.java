public class Main {

    static int[] randPerm(int n) {
        int[] arr = new int[]{};
        int i = 0;
        while (i < n) {
            arr = java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(i)).toArray();
            i = i + 1;
        }
        int idx = n - 1;
        while (idx > 0) {
            int j = Math.floorMod(_now(), (idx + 1));
            int tmp = arr[idx];
arr[idx] = arr[j];
arr[j] = tmp;
            idx = idx - 1;
        }
        return arr;
    }

    static int[] even(int[] xs) {
        int[] r = new int[]{};
        for (int x : xs) {
            if (Math.floorMod(x, 2) == 0) {
                r = java.util.stream.IntStream.concat(java.util.Arrays.stream(r), java.util.stream.IntStream.of(x)).toArray();
            }
        }
        return r;
    }

    static int[] reduceToEven(int[] xs) {
        int[] arr_1 = xs;
        int last = 0;
        int i_1 = 0;
        while (i_1 < arr_1.length) {
            int e = arr_1[i_1];
            if (Math.floorMod(e, 2) == 0) {
arr_1[last] = e;
                last = last + 1;
            }
            i_1 = i_1 + 1;
        }
        return java.util.Arrays.copyOfRange(arr_1, 0, last);
    }

    static String listStr(int[] xs) {
        String s = "[";
        int i_2 = 0;
        while (i_2 < xs.length) {
            s = s + String.valueOf(xs[i_2]);
            if (i_2 + 1 < xs.length) {
                s = s + " ";
            }
            i_2 = i_2 + 1;
        }
        s = s + "]";
        return s;
    }

    static void main() {
        int[] a = randPerm(20);
        int cap_a = 20;
        System.out.println(listStr(a));
        System.out.println(listStr(even(a)));
        System.out.println(listStr(a));
        a = reduceToEven(a);
        System.out.println(listStr(a));
        System.out.println("a len: " + String.valueOf(a.length) + " cap: " + String.valueOf(cap_a));
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
