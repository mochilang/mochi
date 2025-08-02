public class Main {

    static String listStr(int[] xs) {
        String s = "[";
        int i = 0;
        while (i < xs.length) {
            s = s + _p(_geti(xs, i));
            if (i < xs.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static String llStr(int[][] lst) {
        String s_1 = "[";
        int i_1 = 0;
        while (i_1 < lst.length) {
            s_1 = s_1 + String.valueOf(listStr(lst[i_1]));
            if (i_1 < lst.length - 1) {
                s_1 = s_1 + " ";
            }
            i_1 = i_1 + 1;
        }
        s_1 = s_1 + "]";
        return s_1;
    }

    static int[] concat(int[] a, int[] b) {
        int[] out = new int[]{};
        for (int v : a) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(v)).toArray();
        }
        for (int v : b) {
            out = java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(v)).toArray();
        }
        return out;
    }

    static int[][] cartN(Object lists) {
        if ((lists == null)) {
            return new int[][]{};
        }
        int[][] a = _castInt2D(lists);
        if (a.length == 0) {
            return new int[][]{new int[]{}};
        }
        int[][] out_1 = new int[][]{};
        int[][] rest = cartN(java.util.Arrays.copyOfRange(a, 1, a.length));
        for (int x : a[0]) {
            for (int[] p : rest) {
                out_1 = appendObj(out_1, concat(new int[]{x}, p));
            }
        }
        return out_1;
    }

    static void main() {
        System.out.println(llStr(cartN(new int[][]{new int[]{1, 2}, new int[]{3, 4}})));
        System.out.println(llStr(cartN(new int[][]{new int[]{3, 4}, new int[]{1, 2}})));
        System.out.println(llStr(cartN(new int[][]{new int[]{1, 2}, new int[]{}})));
        System.out.println(llStr(cartN(new int[][]{new int[]{}, new int[]{1, 2}})));
        System.out.println("");
        System.out.println("[");
        for (int[] p : cartN(new int[][]{new int[]{1776, 1789}, new int[]{7, 12}, new int[]{4, 14, 23}, new int[]{0, 1}})) {
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
