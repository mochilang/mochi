public class Main {

    static int[] primeFactors(int n) {
        int[] factors = new int[]{};
        int x = n;
        while (Math.floorMod(x, 2) == 0) {
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(2)).toArray();
            x = ((Number)((x / 2))).intValue();
        }
        int p = 3;
        while (p * p <= x) {
            while (Math.floorMod(x, p) == 0) {
                factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(p)).toArray();
                x = ((Number)((x / p))).intValue();
            }
            p = p + 2;
        }
        if (x > 1) {
            factors = java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(x)).toArray();
        }
        return factors;
    }

    static String commatize(int n) {
        String s = String.valueOf(n);
        String out = "";
        int i = s.length() - 1;
        int c = 0;
        while (i >= 0) {
            out = s.substring(i, i + 1) + out;
            c = c + 1;
            if (Math.floorMod(c, 3) == 0 && i > 0) {
                out = "," + out;
            }
            i = i - 1;
        }
        return out;
    }

    static int indexOf(String s, String sub) {
        int i = 0;
        while (i + sub.length() <= s.length()) {
            if ((s.substring(i, i + sub.length()).equals(sub))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String pad10(String s) {
        String str = s;
        while (str.length() < 10) {
            str = " " + str;
        }
        return str;
    }

    static String trimRightStr(String s) {
        int end = s.length();
        while (end > 0 && (s.substring(end - 1, end).equals(" "))) {
            end = end - 1;
        }
        return s.substring(0, end);
    }

    static void main() {
        int[] res = new int[]{};
        int count = 0;
        int k = 11 * 11;
        while (count < 20) {
            if (Math.floorMod(k, 3) == 0 || Math.floorMod(k, 5) == 0 || Math.floorMod(k, 7) == 0) {
                k = k + 2;
                continue;
            }
            int[] factors = primeFactors(k);
            if (factors.length > 1) {
                String s = String.valueOf(k);
                boolean includesAll = true;
                int prev = -1;
                for (int f : factors) {
                    if (f == prev) {
                        continue;
                    }
                    String fs = String.valueOf(f);
                    if (((Number)(s.indexOf(fs))).intValue() == (-1)) {
                        includesAll = false;
                        break;
                    }
                    prev = f;
                }
                if (includesAll) {
                    res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(k)).toArray();
                    count = count + 1;
                }
            }
            k = k + 2;
        }
        String line = "";
        for (int e : java.util.Arrays.copyOfRange(res, 0, 10)) {
            line = line + String.valueOf(pad10(String.valueOf(commatize(e)))) + " ";
        }
        System.out.println(trimRightStr(line));
        line = "";
        for (int e : java.util.Arrays.copyOfRange(res, 10, 20)) {
            line = line + String.valueOf(pad10(String.valueOf(commatize(e)))) + " ";
        }
        System.out.println(trimRightStr(line));
    }
    public static void main(String[] args) {
        main();
    }
}
