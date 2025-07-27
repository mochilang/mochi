public class Main {

    static String listStr(int[] xs) {
        String s = "[";
        int i = 0;
        while (i < xs.length) {
            s = s + String.valueOf(xs[i]);
            if (i < xs.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static String llStr(int[][] lst) {
        String s = "[";
        int i = 0;
        while (i < lst.length) {
            s = s + String.valueOf(listStr(lst[i]));
            if (i < lst.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static int[][] cartN(Object lists) {
        if ((lists == null)) {
            return new int[][]{};
        }
        int[][] a = _castInt2D(lists);
        if (a.length == 0) {
            return new int[][]{new int[]{}};
        }
        int c = 1;
        for (int[] xs : a) {
            c = c * xs.length;
        }
        if (c == 0) {
            return new int[][]{};
        }
        int[][] res = new int[][]{};
        int[] idx = new int[]{};
        for (int[] _v : a) {
            idx = java.util.stream.IntStream.concat(java.util.Arrays.stream(idx), java.util.stream.IntStream.of(0)).toArray();
        }
        int n = a.length;
        int count = 0;
        while (count < c) {
            int[] row = new int[]{};
            int j = 0;
            while (j < n) {
                row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(a[j][idx[j]])).toArray();
                j = j + 1;
            }
            res = appendObj(res, row);
            int k = n - 1;
            while (k >= 0) {
idx[k] = idx[k] + 1;
                if (idx[k] < a[k].length) {
                    break;
                }
idx[k] = 0;
                k = k - 1;
            }
            count = count + 1;
        }
        return res;
    }

    static void main() {
        System.out.println(llStr(cartN(new int[][]{new int[]{1, 2}, new int[]{3, 4}})));
        System.out.println(llStr(cartN(new int[][]{new int[]{3, 4}, new int[]{1, 2}})));
        System.out.println(llStr(cartN(new int[][]{new int[]{1, 2}, new int[]{}})));
        System.out.println(llStr(cartN(new int[][]{new int[]{}, new int[]{1, 2}})));
        System.out.println("");
        System.out.println("[");
        for (var p : cartN(new int[][]{new int[]{1776, 1789}, new int[]{7, 12}, new int[]{4, 14, 23}, new int[]{0, 1}})) {
            System.out.println(" " + String.valueOf(listStr(p)));
        }
        System.out.println("]");
        System.out.println(llStr(cartN(new int[][]{new int[]{1, 2, 3}, new int[]{30}, new int[]{500, 100}})));
        System.out.println(llStr(cartN(new int[][]{new int[]{1, 2, 3}, new int[]{}, new int[]{500, 100}})));
        System.out.println("");
        System.out.println(llStr(cartN(null)));
        System.out.println(llStr(cartN(new Object[]{})));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int[][] _castInt2D(Object v) {
        if (v == null) return new int[][]{};
        if (v instanceof int[][]) return (int[][])v;
        if (v instanceof Object[]) {
            Object[] arr = (Object[])v;
            int[][] out = new int[arr.length][];
            for (int i = 0; i < arr.length; i++) {
                Object e = arr[i];
                if (e instanceof int[]) {
                    out[i] = (int[])e;
                } else if (e instanceof Object[]) {
                    Object[] ar = (Object[])e;
                    int[] ints = new int[ar.length];
                    for (int j = 0; j < ar.length; j++) ints[j] = ((Number)ar[j]).intValue();
                    out[i] = ints;
                } else if (e instanceof Number) {
                    out[i] = new int[]{((Number)e).intValue()};
                } else {
                    out[i] = new int[0];
                }
            }
            return out;
        }
        return new int[][]{};
    }
}
