public class Main {
    static java.util.Map<Integer,Boolean> pps = new java.util.LinkedHashMap<String, Object>();

    static int pow10(int exp) {
        int n = 1;
        int i = 0;
        while (i < exp) {
            n = n * 10;
            i = i + 1;
        }
        return n;
    }

    static int totient(int n) {
        int tot = n;
        int nn = n;
        int i = 2;
        while (i * i <= nn) {
            if (nn % i == 0) {
                while (nn % i == 0) {
                    nn = nn / i;
                }
                tot = tot - tot / i;
            }
            if (i == 2) {
                i = 1;
            }
            i = i + 2;
        }
        if (nn > 1) {
            tot = tot - tot / nn;
        }
        return tot;
    }

    static void getPerfectPowers(int maxExp) {
        int upper = pow10(maxExp);
        int i = 2;
        while (i * i < upper) {
            int p = i;
            while (true) {
                p = p * i;
                if (p >= upper) {
                    break;
                }
pps.put(p, true);
            }
            i = i + 1;
        }
    }

    static java.util.Map<Integer,Boolean> getAchilles(int minExp, int maxExp) {
        int lower = pow10(minExp);
        int upper = pow10(maxExp);
        java.util.Map<Integer,Boolean> achilles = new java.util.LinkedHashMap<String, Object>();
        int b = 1;
        while (b * b * b < upper) {
            int b3 = b * b * b;
            int a = 1;
            while (true) {
                int p = b3 * a * a;
                if (p >= upper) {
                    break;
                }
                if (p >= lower) {
                    if (!(Boolean)(pps.containsKey(p))) {
achilles.put(p, true);
                    }
                }
                a = a + 1;
            }
            b = b + 1;
        }
        return achilles;
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
            res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new int[]{min})).toArray(Object[]::new);
            int[] out = new int[]{};
            int j = 0;
            while (j < tmp.length) {
                if (j != idx) {
                    out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.Arrays.stream(new int[]{tmp[j]})).toArray(Object[]::new);
                }
                j = j + 1;
            }
            tmp = out;
        }
        return res;
    }

    static String pad(int n, int width) {
        String s = String.valueOf(n);
        while (s.length() < width) {
            s = " " + s;
        }
        return s;
    }

    static void main() {
        int maxDigits = 15;
        getPerfectPowers(5);
        java.util.Map<Integer,Boolean> achSet = getAchilles(1, 5);
        int[] ach = new int[]{};
        for (var k : achSet.keys()) {
            ach = java.util.stream.Stream.concat(java.util.Arrays.stream(ach), java.util.Arrays.stream(new int[]{k})).toArray(Object[]::new);
        }
        ach = sortInts(ach);
        System.out.println("First 50 Achilles numbers:");
        int i = 0;
        while (i < 50) {
            String line = "";
            int j = 0;
            while (j < 10) {
                line = line + pad(ach[i], 4);
                if (j < 9) {
                    line = line + " ";
                }
                i = i + 1;
                j = j + 1;
            }
            System.out.println(line);
        }
        System.out.println("\nFirst 30 strong Achilles numbers:");
        int[] strong = new int[]{};
        int count = 0;
        int idx = 0;
        while (count < 30) {
            int tot = totient(ach[idx]);
            if (achSet.containsKey(tot)) {
                strong = java.util.stream.Stream.concat(java.util.Arrays.stream(strong), java.util.Arrays.stream(new int[]{ach[idx]})).toArray(Object[]::new);
                count = count + 1;
            }
            idx = idx + 1;
        }
        i = 0;
        while (i < 30) {
            String line = "";
            int j = 0;
            while (j < 10) {
                line = line + pad(strong[i], 5);
                if (j < 9) {
                    line = line + " ";
                }
                i = i + 1;
                j = j + 1;
            }
            System.out.println(line);
        }
        System.out.println("\nNumber of Achilles numbers with:");
        int[] counts = new int[]{1, 12, 47, 192, 664, 2242, 7395, 24008, 77330, 247449, 788855, 2508051, 7960336, 25235383};
        int d = 2;
        while (d <= maxDigits) {
            int c = counts[d - 2];
            System.out.println(pad(d, 2) + " digits: " + String.valueOf(c));
            d = d + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
