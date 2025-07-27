public class Main {

    static int modPow(int base, int exp, int m) {
        int result = Math.floorMod(1, m);
        int b = Math.floorMod(base, m);
        int e = exp;
        while (e > 0) {
            if (Math.floorMod(e, 2) == 1) {
                result = Math.floorMod((result * b), m);
            }
            b = Math.floorMod((b * b), m);
            e = ((Number)((e / 2))).intValue();
        }
        return result;
    }

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        for (int p : new int[]{2, 3, 5, 7, 11, 13, 17, 19, 23, 29}) {
            if (Math.floorMod(n, p) == 0) {
                return n == p;
            }
        }
        int d = n - 1;
        int s = 0;
        while (Math.floorMod(d, 2) == 0) {
            d = d / 2;
            s = s + String.valueOf(1);
        }
        for (int a : new int[]{2, 325, 9375, 28178, 450775, 9780504, 1795265022}) {
            if (Math.floorMod(a, n) == 0) {
                return true;
            }
            int x = modPow(a, d, n);
            if (x == 1 || x == n - 1) {
                continue;
            }
            int r = 1;
            boolean passed = false;
            while (r < s) {
                x = Math.floorMod((x * x), n);
                if (x == n - 1) {
                    passed = true;
                    break;
                }
                r = r + 1;
            }
            if (!passed) {
                return false;
            }
        }
        return true;
    }

    static String commatize(int n) {
        String s = String.valueOf(n);
        int i = s.length() - 3;
        while (i > 0) {
            s = s.substring(0, i) + "," + s.substring(i, s.length());
            i = i - 3;
        }
        return s;
    }

    static String pad(String s, int width) {
        String out = s;
        while (out.length() < width) {
            out = " " + out;
        }
        return out;
    }

    static String join(String[] xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static String formatRow(String[] row) {
        String[] padded = new String[]{};
        int i = 0;
        while (i < row.length) {
            padded = java.util.stream.Stream.concat(java.util.Arrays.stream(padded), java.util.stream.Stream.of(pad(row[i], 9))).toArray(String[]::new);
            i = i + 1;
        }
        return "[" + String.valueOf(join(padded, " ")) + "]";
    }

    static void main() {
        String[] cubans = new String[]{};
        int cube1 = 1;
        int count = 0;
        int cube100k = 0;
        int i = 1;
        while (true) {
            int j = i + 1;
            int cube2 = j * j * j;
            int diff = cube2 - cube1;
            if (isPrime(diff)) {
                if (count < 200) {
                    cubans = java.util.stream.Stream.concat(java.util.Arrays.stream(cubans), java.util.stream.Stream.of(commatize(diff))).toArray(String[]::new);
                }
                count = count + 1;
                if (count == 100000) {
                    cube100k = diff;
                    break;
                }
            }
            cube1 = cube2;
            i = i + 1;
        }
        System.out.println("The first 200 cuban primes are:-");
        int row = 0;
        while (row < 20) {
            String[] slice = new String[]{};
            int k = 0;
            while (k < 10) {
                slice = java.util.stream.Stream.concat(java.util.Arrays.stream(slice), java.util.stream.Stream.of(cubans[row * 10 + k])).toArray(String[]::new);
                k = k + 1;
            }
            System.out.println(formatRow(slice));
            row = row + 1;
        }
        System.out.println("\nThe 100,000th cuban prime is " + String.valueOf(commatize(cube100k)));
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
