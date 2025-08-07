public class Main {

    static double[] odd_even_transposition(double[] arr) {
        int n = arr.length;
        int pass = 0;
        while (pass < n) {
            int i = Math.floorMod(pass, 2);
            while (i < n - 1) {
                if (arr[i + 1] < arr[i]) {
                    double tmp = arr[i];
arr[i] = arr[i + 1];
arr[i + 1] = tmp;
                }
                i = i + 2;
            }
            pass = pass + 1;
        }
        return arr;
    }
    public static void main(String[] args) {
        System.out.println(_p(odd_even_transposition(((double[])(new double[]{5.0, 4.0, 3.0, 2.0, 1.0})))));
        System.out.println(_p(odd_even_transposition(((double[])(new double[]{13.0, 11.0, 18.0, 0.0, -1.0})))));
        System.out.println(_p(odd_even_transposition(((double[])(new double[]{-0.1, 1.1, 0.1, -2.9})))));
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
