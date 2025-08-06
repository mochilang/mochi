public class Main {

    static int bit_and(int a, int b) {
        int ua = a;
        int ub = b;
        int res = 0;
        int bit = 1;
        while (ua > 0 || ub > 0) {
            if (Math.floorMod(ua, 2) == 1 && Math.floorMod(ub, 2) == 1) {
                res = res + bit;
            }
            ua = ((Number)((ua / 2))).intValue();
            ub = ((Number)((ub / 2))).intValue();
            bit = bit * 2;
        }
        return res;
    }

    static int count_bits_kernighan(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("the value of input must not be negative"));
        }
        int num = n;
        int result = 0;
        while (num != 0) {
            num = bit_and(num, num - 1);
            result = result + 1;
        }
        return result;
    }

    static int count_bits_modulo(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("the value of input must not be negative"));
        }
        int num_1 = n;
        int result_1 = 0;
        while (num_1 != 0) {
            if (Math.floorMod(num_1, 2) == 1) {
                result_1 = result_1 + 1;
            }
            num_1 = ((Number)((num_1 / 2))).intValue();
        }
        return result_1;
    }

    static void main() {
        int[] numbers = ((int[])(new int[]{25, 37, 21, 58, 0, 256}));
        int i = 0;
        while (i < numbers.length) {
            System.out.println(_p(count_bits_kernighan(numbers[i])));
            i = i + 1;
        }
        i = 0;
        while (i < numbers.length) {
            System.out.println(_p(count_bits_modulo(numbers[i])));
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
    }

    static String _repeat(String s, int n) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) sb.append(s);
        return sb.toString();
    }

    static void json(Object v) {
        System.out.println(_json(v));
    }

    static String _json(Object v) {
        if (v == null) return "null";
        if (v instanceof String) {
            String s = (String)v;
            s = s.replace("\\", "\\\\").replace("\"", "\\\"");
            return "\"" + s + "\"";
        }
        if (v instanceof Number || v instanceof Boolean) {
            return String.valueOf(v);
        }
        if (v instanceof int[]) {
            int[] a = (int[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof double[]) {
            double[] a = (double[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof boolean[]) {
            boolean[] a = (boolean[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(a[i]); }
            sb.append("]");
            return sb.toString();
        }
        if (v.getClass().isArray()) {
            Object[] a = (Object[]) v;
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < a.length; i++) { if (i > 0) sb.append(","); sb.append(_json(a[i])); }
            sb.append("]");
            return sb.toString();
        }
        String s = String.valueOf(v);
        s = s.replace("\\", "\\\\").replace("\"", "\\\"");
        return "\"" + s + "\"";
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
