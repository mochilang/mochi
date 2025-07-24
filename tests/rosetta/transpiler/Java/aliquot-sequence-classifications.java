public class Main {
    static long THRESHOLD = 140737488355328L;

    static long indexOf(long[] xs, long value) {
        long i = 0;
        while (i < xs.length) {
            if (xs[(int)(i)] == value) {
                return i;
            }
            i = i + 1;
        }
        return 0 - 1;
    }

    static boolean contains(long[] xs, long value) {
        return indexOf(xs, value) != 0 - 1;
    }

    static long maxOf(long a, long b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    static long intSqrt(long n) {
        if (n == 0) {
            return 0;
        }
        long x = n;
        long y = ((Number)((x + 1))).doubleValue() / 2;
        while (y < x) {
            x = y;
            y = ((Number)((x + n / x))).doubleValue() / 2;
        }
        return x;
    }

    static long sumProperDivisors(long n) {
        if (n < 2) {
            return 0;
        }
        long sqrt = intSqrt(n);
        long sum = 1;
        long i = 2;
        while (i <= sqrt) {
            if (n % i == 0) {
                sum = sum + i + n / i;
            }
            i = i + 1;
        }
        if (sqrt * sqrt == n) {
            sum = sum - sqrt;
        }
        return sum;
    }

    static java.util.Map<String,Object> classifySequence(long k) {
        long last = k;
        long[] seq = new long[]{k};
        while (true) {
            last = sumProperDivisors(last);
            seq = java.util.stream.LongStream.concat(java.util.Arrays.stream(seq), java.util.stream.LongStream.of(last)).toArray();
            long n = seq.length;
            String aliquot = "";
            if (last == 0) {
                aliquot = "Terminating";
            } else             if (n == 2 && last == k) {
                aliquot = "Perfect";
            } else             if (n == 3 && last == k) {
                aliquot = "Amicable";
            } else             if (n >= 4 && last == k) {
                aliquot = "Sociable[" + String.valueOf(n - 1) + "]";
            } else             if (last == seq[(int)(n - 2)]) {
                aliquot = "Aspiring";
            } else             if (contains(java.util.Arrays.copyOfRange(seq, 1, maxOf(1, n - 2)), last)) {
                long idx = indexOf(seq, last);
                aliquot = "Cyclic[" + String.valueOf(n - 1 - idx) + "]";
            } else             if (n == 16 || last > THRESHOLD) {
                aliquot = "Non-Terminating";
            }
            if (!(aliquot.equals(""))) {
                return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("seq", seq, "aliquot", aliquot));
            }
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("seq", seq, "aliquot", ""));
    }

    static String padLeft(long n, long w) {
        String s = String.valueOf(n);
        while (s.length() < w) {
            s = " " + s;
        }
        return s;
    }

    static String padRight(String s, long w) {
        String r = s;
        while (r.length() < w) {
            r = r + " ";
        }
        return r;
    }

    static String joinWithCommas(long[] seq) {
        String s = "[";
        long i = 0;
        while (i < seq.length) {
            s = s + String.valueOf(seq[(int)(i)]);
            if (i < seq.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static void main() {
        System.out.println("Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n");
        long k = 1;
        while (k <= 10) {
            java.util.Map<String,Object> res = classifySequence(k);
            System.out.println(padLeft(k, 2) + ": " + padRight((String)(((String)res.get("aliquot"))), 15) + " " + joinWithCommas((long[])(((long[])res.get("seq")))));
            k = k + 1;
        }
        System.out.println("");
        long[] s = new long[]{11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488};
        long i = 0;
        while (i < s.length) {
            long val = s[(int)(i)];
            java.util.Map<String,Object> res = classifySequence(val);
            System.out.println(padLeft(val, 7) + ": " + padRight((String)(((String)res.get("aliquot"))), 15) + " " + joinWithCommas((long[])(((long[])res.get("seq")))));
            i = i + 1;
        }
        System.out.println("");
        long big = 15355717786080L;
        java.util.Map<String,Object> r = classifySequence(big);
        System.out.println(String.valueOf(big) + ": " + padRight((String)(((String)r.get("aliquot"))), 15) + " " + joinWithCommas((long[])(((long[])r.get("seq")))));
    }
    public static void main(String[] args) {
        main();
    }
}
