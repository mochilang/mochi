public class Main {
    static int THRESHOLD = 140737488355328;

    static int indexOf(int[] xs, int value) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == value) {
                return i;
            }
            i = i + 1;
        }
        return 0 - 1;
    }

    static boolean contains(int[] xs, int value) {
        return indexOf(xs, value) != 0 - 1;
    }

    static int maxOf(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }

    static int intSqrt(int n) {
        if (n == 0) {
            return 0;
        }
        int x = n;
        int y = (x + 1) / 2;
        while (y < x) {
            x = y;
            y = (x + n / x) / 2;
        }
        return x;
    }

    static int sumProperDivisors(int n) {
        if (n < 2) {
            return 0;
        }
        int sqrt = intSqrt(n);
        int sum = 1;
        int i = 2;
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

    static java.util.Map<String,Object> classifySequence(int k) {
        int last = k;
        int[] seq = new int[]{k};
        while (true) {
            last = sumProperDivisors(last);
            seq = java.util.stream.IntStream.concat(java.util.Arrays.stream(seq), java.util.stream.IntStream.of(last)).toArray();
            int n = seq.length;
            String aliquot = "";
            if (last == 0) {
                aliquot = "Terminating";
            } else             if (n == 2 && last == k) {
                aliquot = "Perfect";
            } else             if (n == 3 && last == k) {
                aliquot = "Amicable";
            } else             if (n >= 4 && last == k) {
                aliquot = "Sociable[" + String.valueOf(n - 1) + "]";
            } else             if (last == seq[n - 2]) {
                aliquot = "Aspiring";
            } else             if (contains(java.util.Arrays.copyOfRange(seq, 1, maxOf(1, n - 2)), last)) {
                int idx = indexOf(seq, last);
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

    static String padLeft(int n, int w) {
        String s = String.valueOf(n);
        while (s.length() < w) {
            s = " " + s;
        }
        return s;
    }

    static String padRight(String s, int w) {
        String r = s;
        while (r.length() < w) {
            r = r + " ";
        }
        return r;
    }

    static String joinWithCommas(int[] seq) {
        String s = "[";
        int i = 0;
        while (i < seq.length) {
            s = s + String.valueOf(seq[i]);
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
        int k = 1;
        while (k <= 10) {
            java.util.Map<String,Object> res = classifySequence(k);
            System.out.println(padLeft(k, 2) + ": " + padRight((String)(res.get("aliquot")), 15) + " " + joinWithCommas((int[])(res.get("seq"))));
            k = k + 1;
        }
        System.out.println("");
        int[] s = new int[]{11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488};
        int i = 0;
        while (i < s.length) {
            int val = s[i];
            java.util.Map<String,Object> res = classifySequence(val);
            System.out.println(padLeft(val, 7) + ": " + padRight((String)(res.get("aliquot")), 15) + " " + joinWithCommas((int[])(res.get("seq"))));
            i = i + 1;
        }
        System.out.println("");
        int big = 15355717786080;
        java.util.Map<String,Object> r = classifySequence(big);
        System.out.println(String.valueOf(big) + ": " + padRight((String)(r.get("aliquot")), 15) + " " + joinWithCommas((int[])(r.get("seq"))));
    }
    public static void main(String[] args) {
        main();
    }
}
