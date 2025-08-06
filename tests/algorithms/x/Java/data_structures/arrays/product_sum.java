public class Main {
    static Object[] example;

    static int product_sum(Object[] arr, int depth) {
        int total = 0;
        int i = 0;
        while (i < arr.length) {
            Object el = arr[i];
            if (((Boolean)(_exists(el)))) {
                total = total + product_sum(((Object[])(_toObjectArray(el))), depth + 1);
            } else {
                total = total + ((Number)(el)).intValue();
            }
            i = i + 1;
        }
        return total * depth;
    }

    static int product_sum_array(Object[] array) {
        int res = product_sum(((Object[])(array)), 1);
        return res;
    }
    public static void main(String[] args) {
        example = new Object[]{5, 2, new int[]{-7, 1}, 3, new Object[]{6, new int[]{-13, 8}, 4}};
        System.out.println(product_sum_array(((Object[])(example))));
    }

    static boolean _exists(Object v) {
        if (v == null) return false;
        if (v instanceof java.util.List<?>) return true;
        return v.getClass().isArray();
    }

    static Object[] _toObjectArray(Object v) {
        if (v instanceof Object[]) return (Object[]) v;
        if (v instanceof int[]) return java.util.Arrays.stream((int[]) v).boxed().toArray();
        if (v instanceof double[]) return java.util.Arrays.stream((double[]) v).boxed().toArray();
        if (v instanceof long[]) return java.util.Arrays.stream((long[]) v).boxed().toArray();
        if (v instanceof boolean[]) { boolean[] a = (boolean[]) v; Object[] out = new Object[a.length]; for (int i = 0; i < a.length; i++) out[i] = a[i]; return out; }
        return (Object[]) v;
    }
}
