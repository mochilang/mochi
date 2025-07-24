public class Main {
    static int x = 1;

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

    static String bigToString(int[] a) {
        String s = "";
        int i = a.length - 1;
        while (i >= 0) {
            s = s + String.valueOf(a[i]);
            i = i - 1;
        }
        return s;
    }

    static int minInt(int a, int b) {
        if (a < b) {
            return a;
        } else {
            return b;
        }
    }

    static int[][] cumu(int n) {
        int[][][] cache = new int[][][]{new int[][]{bigFromInt(1)}};
        int y = 1;
        while (y <= n) {
            int[][] row = new int[][]{bigFromInt(0)};
            int x = 1;
            while (x <= y) {
                int[] val = cache[y - x][minInt(x, y - x)];
                row = appendObj(row, bigAdd(row[row.length - 1], val));
                x = x + 1;
            }
            cache = appendObj(cache, row);
            y = y + 1;
        }
        return cache[n];
    }

    static String[] row(int n) {
        int[][] e = cumu(n);
        String[] out = new String[]{};
        int i = 0;
        while (i < n) {
            int[] diff = bigSub(e[i + 1], e[i]);
            out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(bigToString(diff))).toArray(String[]::new);
            i = i + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        System.out.println("rows:");
        while (x < 11) {
            String[] r = row(x);
            String line = "";
            int i = 0;
            while (i < r.length) {
                line = line + " " + r[i] + " ";
                i = i + 1;
            }
            System.out.println(line);
            x = x + 1;
        }
        System.out.println("");
        System.out.println("sums:");
        for (var num : new int[]{23, 123, 1234}) {
            int[][] r = cumu(num);
            System.out.println(String.valueOf(num) + " " + bigToString(r[r.length - 1]));
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
