public class Main {

    static java.util.Map<String,Double> ohms_law(double voltage, double current, double resistance) {
        java.math.BigInteger zeros = java.math.BigInteger.valueOf(0);
        if ((double)(voltage) == (double)(0.0)) {
            zeros = new java.math.BigInteger(String.valueOf(zeros.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(current) == (double)(0.0)) {
            zeros = new java.math.BigInteger(String.valueOf(zeros.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(resistance) == (double)(0.0)) {
            zeros = new java.math.BigInteger(String.valueOf(zeros.add(java.math.BigInteger.valueOf(1))));
        }
        if (zeros.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            System.out.println("One and only one argument must be 0");
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        }
        if ((double)(resistance) < (double)(0.0)) {
            System.out.println("Resistance cannot be negative");
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        }
        if ((double)(voltage) == (double)(0.0)) {
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("voltage", (double)((double)(current) * (double)(resistance))); }}));
        }
        if ((double)(current) == (double)(0.0)) {
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("current", (double)((double)(voltage) / (double)(resistance))); }}));
        }
        return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("resistance", (double)((double)(voltage) / (double)(current))); }}));
    }
    public static void main(String[] args) {
        json(ohms_law((double)(10.0), (double)(0.0), (double)(5.0)));
        json(ohms_law((double)(-10.0), (double)(1.0), (double)(0.0)));
        json(ohms_law((double)(0.0), (double)(-1.5), (double)(2.0)));
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
