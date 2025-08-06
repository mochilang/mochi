public class Main {

    static int bitwise_xor(int a, int b) {
        int result = 0;
        int bit = 1;
        int x = a;
        int y = b;
        while (x > 0 || y > 0) {
            int ax = Math.floorMod(x, 2);
            int by = Math.floorMod(y, 2);
            if (Math.floorMod((ax + by), 2) == 1) {
                result = result + bit;
            }
            x = x / 2;
            y = y / 2;
            bit = bit * 2;
        }
        return result;
    }

    static int bitwise_and(int a, int b) {
        int result_1 = 0;
        int bit_1 = 1;
        int x_1 = a;
        int y_1 = b;
        while (x_1 > 0 && y_1 > 0) {
            if (Math.floorMod(x_1, 2) == 1 && Math.floorMod(y_1, 2) == 1) {
                result_1 = result_1 + bit_1;
            }
            x_1 = x_1 / 2;
            y_1 = y_1 / 2;
            bit_1 = bit_1 * 2;
        }
        return result_1;
    }

    static int bitwise_addition_recursive(int number, int other_number) {
        if (number < 0 || other_number < 0) {
            throw new RuntimeException(String.valueOf("Both arguments MUST be non-negative!"));
        }
        int bitwise_sum = bitwise_xor(number, other_number);
        int carry = bitwise_and(number, other_number);
        if (carry == 0) {
            return bitwise_sum;
        }
        return bitwise_addition_recursive(bitwise_sum, carry * 2);
    }
    public static void main(String[] args) {
        System.out.println(_p(bitwise_addition_recursive(4, 5)));
        System.out.println(_p(bitwise_addition_recursive(8, 9)));
        System.out.println(_p(bitwise_addition_recursive(0, 4)));
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
