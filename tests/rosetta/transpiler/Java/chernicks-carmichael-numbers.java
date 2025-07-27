public class Main {

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        if (Math.floorMod(n, 3) == 0) {
            return n == 3;
        }
        int d = 5;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 4;
        }
        return true;
    }

    static int[] bigTrim(int[] a) {
        int n = a.length;
        while (n > 1 && a[n - 1] == 0) {
            a = java.util.Arrays.copyOfRange(a, 0, n - 1);
            n = n - 1;
        }
        return a;
    }

    static int[] bigFromInt(int x) {
        if (x == 0) {
            return new int[]{0};
        }
        int[] digits = new int[]{};
        int n = x;
        while (n > 0) {
            digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(Math.floorMod(n, 10))).toArray();
            n = n / 10;
        }
        return digits;
    }

    static int[] bigMulSmall(int[] a, int m) {
        if (m == 0) {
            return new int[]{0};
        }
        int[] res = new int[]{};
        int carry = 0;
        int i = 0;
        while (i < a.length) {
            int prod = a[i] * m + carry;
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(Math.floorMod(prod, 10))).toArray();
            carry = prod / 10;
            i = i + 1;
        }
        while (carry > 0) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(Math.floorMod(carry, 10))).toArray();
            carry = carry / 10;
        }
        return bigTrim(res);
    }

    static String bigToString(int[] a) {
        String s = "";
        int i = a.length - 1;
        while (i >= 0) {
            s = s + String.valueOf(a[i]);
            i = i - 1;
        }
        return s;
    }

    static int pow2(int k) {
        int r = 1;
        int i = 0;
        while (i < k) {
            r = r * 2;
            i = i + 1;
        }
        return r;
    }

    static int[] ccFactors(int n, int m) {
        int p = 6 * m + 1;
        if (!(Boolean)isPrime(p)) {
            return new int[]{};
        }
        int[] prod = bigFromInt(p);
        p = 12 * m + 1;
        if (!(Boolean)isPrime(p)) {
            return new int[]{};
        }
        prod = bigMulSmall(prod, p);
        int i = 1;
        while (i <= n - 2) {
            p = (pow2(i) * 9 * m) + 1;
            if (!(Boolean)isPrime(p)) {
                return new int[]{};
            }
            prod = bigMulSmall(prod, p);
            i = i + 1;
        }
        return prod;
    }

    static void ccNumbers(int start, int end) {
        int n = start;
        while (n <= end) {
            int m = 1;
            if (n > 4) {
                m = pow2(n - 4);
            }
            while (true) {
                int[] num = ccFactors(n, m);
                if (num.length > 0) {
                    System.out.println("a(" + String.valueOf(n) + ") = " + String.valueOf(bigToString(num)));
                    break;
                }
                if (n <= 4) {
                    m = m + 1;
                } else {
                    m = m + pow2(n - 4);
                }
            }
            n = n + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ccNumbers(3, 9);
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
