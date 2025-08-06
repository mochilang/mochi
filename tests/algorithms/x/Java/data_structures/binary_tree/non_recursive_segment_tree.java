public class Main {
    static int[] arr1;
    static int[] st1;
    static int[] arr2;
    static int[] st2;
    static int[] arr3;
    static int[] st3;
    static int[] arr4;
    static int n4;
    static int[] st4;

    static int[] build(int[] arr, java.util.function.BiFunction<Integer,Integer,Integer> combine) {
        int n = arr.length;
        int[] st = ((int[])(new int[]{}));
        int i = 0;
        while (i < 2 * n) {
            st = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(st), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        i = 0;
        while (i < n) {
st[n + i] = arr[i];
            i = i + 1;
        }
        i = n - 1;
        while (i > 0) {
st[i] = combine.apply(st[i * 2], st[i * 2 + 1]);
            i = i - 1;
        }
        return st;
    }

    static void update(int[] st, int n, java.util.function.BiFunction<Integer,Integer,Integer> combine, int p, int v) {
        int idx = p + n;
st[idx] = v;
        while (idx > 1) {
            idx = ((Number)((idx / 2))).intValue();
st[idx] = combine.apply(st[idx * 2], st[idx * 2 + 1]);
        }
    }

    static int query(int[] st, int n, java.util.function.BiFunction<Integer,Integer,Integer> combine, int left, int right) {
        int l = left + n;
        int r = right + n;
        int res = 0;
        boolean has = false;
        while (l <= r) {
            if (Math.floorMod(l, 2) == 1) {
                if (!has) {
                    res = st[l];
                    has = true;
                } else {
                    res = combine.apply(res, st[l]);
                }
                l = l + 1;
            }
            if (Math.floorMod(r, 2) == 0) {
                if (!has) {
                    res = st[r];
                    has = true;
                } else {
                    res = combine.apply(res, st[r]);
                }
                r = r - 1;
            }
            l = ((Number)((l / 2))).intValue();
            r = ((Number)((r / 2))).intValue();
        }
        return res;
    }

    static int add(int a, int b) {
        return a + b;
    }

    static int min_int(int a, int b) {
        if (a < b) {
            return a;
        } else {
            return b;
        }
    }

    static int max_int(int a, int b) {
        if (a > b) {
            return a;
        } else {
            return b;
        }
    }
    public static void main(String[] args) {
        arr1 = ((int[])(new int[]{1, 2, 3}));
        st1 = ((int[])(build(((int[])(arr1)), Main::add)));
        System.out.println(_p(query(((int[])(st1)), arr1.length, Main::add, 0, 2)));
        arr2 = ((int[])(new int[]{3, 1, 2}));
        st2 = ((int[])(build(((int[])(arr2)), Main::min_int)));
        System.out.println(_p(query(((int[])(st2)), arr2.length, Main::min_int, 0, 2)));
        arr3 = ((int[])(new int[]{2, 3, 1}));
        st3 = ((int[])(build(((int[])(arr3)), Main::max_int)));
        System.out.println(_p(query(((int[])(st3)), arr3.length, Main::max_int, 0, 2)));
        arr4 = ((int[])(new int[]{1, 5, 7, -1, 6}));
        n4 = arr4.length;
        st4 = ((int[])(build(((int[])(arr4)), Main::add)));
        update(((int[])(st4)), n4, Main::add, 1, -1);
        update(((int[])(st4)), n4, Main::add, 2, 3);
        System.out.println(_p(query(((int[])(st4)), n4, Main::add, 1, 2)));
        System.out.println(_p(query(((int[])(st4)), n4, Main::add, 1, 1)));
        update(((int[])(st4)), n4, Main::add, 4, 1);
        System.out.println(_p(query(((int[])(st4)), n4, Main::add, 3, 4)));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
