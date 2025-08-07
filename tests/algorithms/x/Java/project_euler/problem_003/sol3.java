public class Main {

    static int largest_prime_factor(int n) {
        if (n <= 1) {
            return n;
        }
        int i = 2;
        int ans = 0;
        int m = n;
        if (m == 2) {
            return 2;
        }
        while (m > 2) {
            while (Math.floorMod(m, i) != 0) {
                i = i + 1;
            }
            ans = i;
            while (Math.floorMod(m, i) == 0) {
                m = Math.floorDiv(m, i);
            }
            i = i + 1;
        }
        return ans;
    }
    public static void main(String[] args) {
        json(largest_prime_factor(13195));
        json(largest_prime_factor(10));
        json(largest_prime_factor(17));
        json(largest_prime_factor((int)600851475143L));
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
}
