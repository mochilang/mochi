public class Main {

    static long[][] cart2(long[] a, long[] b) {
        long[][] p = new long[][]{};
        for (long x : a) {
            for (long y : b) {
                p = appendObj(p, new long[]{x, y});
            }
        }
        return p;
    }

    static String llStr(long[][] lst) {
        String s = "[";
        long i = 0L;
        while (i < lst.length) {
            long[] row = lst[(int)(i)];
            s = s + "[";
            long j = 0L;
            while (j < row.length) {
                s = s + String.valueOf(row[(int)(j)]);
                if (j < row.length - 1L) {
                    s = s + " ";
                }
                j = j + 1L;
            }
            s = s + "]";
            if (i < lst.length - 1L) {
                s = s + " ";
            }
            i = i + 1L;
        }
        s = s + "]";
        return s;
    }

    static void main() {
        System.out.println(llStr(cart2(new long[]{1L, 2L}, new long[]{3L, 4L})));
        System.out.println(llStr(cart2(new long[]{3L, 4L}, new long[]{1L, 2L})));
        System.out.println(llStr(cart2(new long[]{1L, 2L}, new long[]{})));
        System.out.println(llStr(cart2(new long[]{}, new long[]{1L, 2L})));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
