public class Main {
    static int seed = 1;

    static int prng(int max) {
        seed = Math.floorMod((seed * 1103515245 + 12345), (int)2147483648L);
        return Math.floorMod(seed, max);
    }

    static String gen(int n) {
        String[] arr = new String[]{};
        int i = 0;
        while (i < n) {
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of("[")).toArray(String[]::new);
            arr = java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of("]")).toArray(String[]::new);
            i = i + 1;
        }
        int j = arr.length - 1;
        while (j > 0) {
            int k = prng(j + 1);
            String tmp = String.valueOf(arr[j]);
arr[j] = arr[k];
arr[k] = tmp;
            j = j - 1;
        }
        String out = "";
        for (String ch : arr) {
            out = String.valueOf(out + ch);
        }
        return out;
    }

    static void testBalanced(String s) {
        int open = 0;
        int i = 0;
        while (i < s.length()) {
            String c = s.substring(i, i + 1);
            if ((c.equals("["))) {
                open = open + 1;
            } else             if ((c.equals("]"))) {
                if (open == 0) {
                    System.out.println(s + ": not ok");
                    return;
                }
                open = open - 1;
            } else {
                System.out.println(s + ": not ok");
                return;
            }
            i = i + 1;
        }
        if (open == 0) {
            System.out.println(s + ": ok");
        } else {
            System.out.println(s + ": not ok");
        }
    }

    static void main() {
        int i = 0;
        while (i < 10) {
            testBalanced(String.valueOf(gen(i)));
            i = i + 1;
        }
        testBalanced("()");
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
