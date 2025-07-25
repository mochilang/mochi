public class Main {

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
            digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(n % 10)).toArray();
            n = n / 10;
        }
        return digits;
    }

    static int bigCmp(int[] a, int[] b) {
        if (a.length > b.length) {
            return 1;
        }
        if (a.length < b.length) {
            return -1;
        }
        int i = a.length - 1;
        while (i >= 0) {
            if (((Number)(a[i])).intValue() > ((Number)(b[i])).intValue()) {
                return 1;
            }
            if (((Number)(a[i])).intValue() < ((Number)(b[i])).intValue()) {
                return -1;
            }
            i = i - 1;
        }
        return 0;
    }

    static int[] bigAdd(int[] a, int[] b) {
        int[] res = new int[]{};
        int carry = 0;
        int i = 0;
        while (i < a.length || i < b.length || carry > 0) {
            int av = 0;
            if (i < a.length) {
                av = a[i];
            }
            int bv = 0;
            if (i < b.length) {
                bv = b[i];
            }
            int s = av + bv + carry;
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(s % 10)).toArray();
            carry = s / 10;
            i = i + 1;
        }
        return bigTrim(res);
    }

    static int[] bigSub(int[] a, int[] b) {
        int[] res = new int[]{};
        int borrow = 0;
        int i = 0;
        while (i < a.length) {
            int av = a[i];
            int bv = 0;
            if (i < b.length) {
                bv = b[i];
            }
            int diff = av - bv - borrow;
            if (diff < 0) {
                diff = diff + 10;
                borrow = 1;
            } else {
                borrow = 0;
            }
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(diff)).toArray();
            i = i + 1;
        }
        return bigTrim(res);
    }

    static int[] bigMulSmall(int[] a, int m) {
        if (m == 0) {
            return new int[]{0};
        }
        int[] res = new int[]{};
        int carry = 0;
        int i = 0;
        while (i < a.length) {
            int prod = ((Number)(a[i])).intValue() * m + carry;
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(prod % 10)).toArray();
            carry = prod / 10;
            i = i + 1;
        }
        while (carry > 0) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(carry % 10)).toArray();
            carry = carry / 10;
        }
        return bigTrim(res);
    }

    static int[] bigMulBig(int[] a, int[] b) {
        int[] res = new int[]{};
        int i = 0;
        while (i < a.length + b.length) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
        i = 0;
        while (i < a.length) {
            int carry = 0;
            int j = 0;
            while (j < b.length) {
                int idx = i + j;
                int prod = res[idx] + ((Number)(a[i])).intValue() * ((Number)(b[j])).intValue() + carry;
res[idx] = prod % 10;
                carry = prod / 10;
                j = j + 1;
            }
            int idx = i + b.length;
            while (carry > 0) {
                int prod = res[idx] + carry;
res[idx] = prod % 10;
                carry = prod / 10;
                idx = idx + 1;
            }
            i = i + 1;
        }
        return bigTrim(res);
    }

    static int[] bigMulPow10(int[] a, int k) {
        int i = 0;
        while (i < k) {
            a = java.util.stream.IntStream.concat(java.util.Arrays.stream(new int[]{0}), java.util.Arrays.stream(a)).toArray();
            i = i + 1;
        }
        return a;
    }

    static int[] bigDivSmall(int[] a, int m) {
        int[] res = new int[]{};
        int rem = 0;
        int i = a.length - 1;
        while (i >= 0) {
            int cur = rem * 10 + ((Number)(a[i])).intValue();
            int q = cur / m;
            rem = cur % m;
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(new int[]{q}), java.util.Arrays.stream(res)).toArray();
            i = i - 1;
        }
        return bigTrim(res);
    }

    static String bigToString(int[] a) {
        String s = "";
        int i = a.length - 1;
        while (i >= 0) {
            s = String.valueOf(s + String.valueOf(a[i]));
            i = i - 1;
        }
        return s;
    }

    static String repeat(String ch, int n) {
        String s = "";
        int i = 0;
        while (i < n) {
            s = String.valueOf(s + ch);
            i = i + 1;
        }
        return s;
    }

    static int[] sortInts(int[] xs) {
        int[] res = new int[]{};
        int[] tmp = xs;
        while (tmp.length > 0) {
            int min = tmp[0];
            int idx = 0;
            int i = 1;
            while (i < tmp.length) {
                if (tmp[i] < min) {
                    min = tmp[i];
                    idx = i;
                }
                i = i + 1;
            }
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new int[]{min})).toArray();
            int[] out = new int[]{};
            int j = 0;
            while (j < tmp.length) {
                if (j != idx) {
                    out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.Arrays.stream(new int[]{tmp[j]})).toArray();
                }
                j = j + 1;
            }
            tmp = out;
        }
        return res;
    }

    static int[] primesUpTo(int n) {
        boolean[] sieve = new boolean[]{};
        int i = 0;
        while (i <= n) {
            sieve = appendBool(sieve, true);
            i = i + 1;
        }
        int p = 2;
        while (p * p <= n) {
            if (sieve[p]) {
                int m = p * p;
                while (m <= n) {
sieve[m] = false;
                    m = m + p;
                }
            }
            p = p + 1;
        }
        int[] res = new int[]{};
        int x = 2;
        while (x <= n) {
            if (sieve[x]) {
                res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(x)).toArray();
            }
            x = x + 1;
        }
        return res;
    }

    static java.util.Map<String,Integer> factorialExp(int n, int[] primes) {
        java.util.Map<String,Integer> m = new java.util.LinkedHashMap<String, Integer>();
        for (var p : primes) {
            if (p > n) {
                break;
            }
            int t = n;
            int e = 0;
            while (t > 0) {
                t = t / p;
                e = e + t;
            }
m.put(String.valueOf(p), e);
        }
        return m;
    }

    static java.util.Map<String,Integer> factorSmall(int x, int[] primes) {
        java.util.Map<String,Integer> f = new java.util.LinkedHashMap<String, Integer>();
        int n = x;
        for (var p : primes) {
            if (p * p > n) {
                break;
            }
            int c = 0;
            while (n % p == 0) {
                c = c + 1;
                n = n / p;
            }
            if (c > 0) {
f.put(String.valueOf(p), c);
            }
        }
        if (n > 1) {
f.put(String.valueOf(n), ((Number)(f.getOrDefault(String.valueOf(n), 0))).intValue() + 1);
        }
        return f;
    }

    static int[] computeIP(int n, int[] primes) {
        java.util.Map<String,Integer> exps = factorialExp(6 * n, primes);
        java.util.Map<String,Integer> fn = factorialExp(n, primes);
        for (var k : fn.keySet()) {
exps.put(k, ((Number)(exps.getOrDefault(k, 0))).intValue() - 6 * (int)(((int)fn.get(k))));
        }
exps.put("2", ((Number)(exps.getOrDefault("2", 0))).intValue() + 5);
        int t2 = 532 * n * n + 126 * n + 9;
        java.util.Map<String,Integer> ft2 = factorSmall(t2, primes);
        for (var k : ft2.keySet()) {
exps.put(k, ((Number)(exps.getOrDefault(k, 0))).intValue() + (int)(((int)ft2.get(k))));
        }
exps.put("3", ((Number)(exps.getOrDefault("3", 0))).intValue() - 1);
        int[] keys = new int[]{};
        for (var k : exps.keySet()) {
            keys = java.util.stream.IntStream.concat(java.util.Arrays.stream(keys), java.util.stream.IntStream.of(Integer.parseInt(k))).toArray();
        }
        keys = sortInts(keys);
        int[] res = bigFromInt(1);
        for (var p : keys) {
            int e = (int)(((int)exps.get(String.valueOf(p))));
            int i = 0;
            while (i < e) {
                res = bigMulSmall(res, p);
                i = i + 1;
            }
        }
        return res;
    }

    static String formatTerm(int[] ip, int pw) {
        String s = String.valueOf(bigToString(ip));
        if (pw >= s.length()) {
            String frac = String.valueOf(String.valueOf(repeat("0", pw - s.length())) + s);
            if (frac.length() < 33) {
                frac = String.valueOf(frac + String.valueOf(repeat("0", 33 - frac.length())));
            }
            return "0." + frac.substring(0, 33);
        }
        String intpart = s.substring(0, s.length() - pw);
        String frac = s.substring(s.length() - pw, s.length());
        if (frac.length() < 33) {
            frac = String.valueOf(frac + String.valueOf(repeat("0", 33 - frac.length())));
        }
        return String.valueOf(intpart + ".") + frac.substring(0, 33);
    }

    static int[] bigAbsDiff(int[] a, int[] b) {
        if (bigCmp(a, b) >= 0) {
            return bigSub(a, b);
        }
        return bigSub(b, a);
    }

    static void main() {
        int[] primes = primesUpTo(2000);
        System.out.println("N                               Integer Portion  Pow  Nth Term (33 dp)");
        String line = String.valueOf(repeat("-", 89));
        System.out.println(line);
        int[] sum = bigFromInt(0);
        int[] prev = bigFromInt(0);
        int denomPow = 0;
        int n = 0;
        while (true) {
            int[] ip = computeIP(n, primes);
            int pw = 6 * n + 3;
            if (pw > denomPow) {
                sum = bigMulPow10(sum, pw - denomPow);
                prev = bigMulPow10(prev, pw - denomPow);
                denomPow = pw;
            }
            if (n < 10) {
                String termStr = String.valueOf(formatTerm(ip, pw));
                String ipStr = String.valueOf(bigToString(ip));
                while (ipStr.length() < 44) {
                    ipStr = String.valueOf(" " + ipStr);
                }
                String pwStr = String.valueOf(-pw);
                while (pwStr.length() < 3) {
                    pwStr = String.valueOf(" " + pwStr);
                }
                String padTerm = termStr;
                while (padTerm.length() < 35) {
                    padTerm = String.valueOf(padTerm + " ");
                }
                System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf(n) + "  ") + ipStr) + "  ") + pwStr) + "  ") + padTerm);
            }
            sum = bigAdd(sum, ip);
            int[] diff = bigAbsDiff(sum, prev);
            if (denomPow >= 70 && bigCmp(diff, bigMulPow10(bigFromInt(1), denomPow - 70)) < 0) {
                break;
            }
            prev = sum;
            n = n + 1;
        }
        int precision = 70;
        int[] target = bigMulPow10(bigFromInt(1), denomPow + 2 * precision);
        int[] low = bigFromInt(0);
        int[] high = bigMulPow10(bigFromInt(1), precision + 1);
        while (bigCmp(low, bigSub(high, bigFromInt(1))) < 0) {
            int[] mid = bigDivSmall(bigAdd(low, high), 2);
            int[] prod = bigMulBig(bigMulBig(mid, mid), sum);
            if (bigCmp(prod, target) <= 0) {
                low = mid;
            } else {
                high = bigSub(mid, bigFromInt(1));
            }
        }
        int[] piInt = low;
        String piStr = String.valueOf(bigToString(piInt));
        if (piStr.length() <= precision) {
            piStr = String.valueOf(String.valueOf(repeat("0", precision - piStr.length() + 1)) + piStr);
        }
        String out = String.valueOf(String.valueOf(piStr.substring(0, piStr.length() - precision) + ".") + piStr.substring(piStr.length() - precision, piStr.length()));
        System.out.println("");
        System.out.println("Pi to 70 decimal places is:");
        System.out.println(out);
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
