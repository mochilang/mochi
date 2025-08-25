public class Main {
    static double PI = (double)(3.141592653589793);

    static java.util.Map<String,Double> ind_reactance(double inductance, double frequency, double reactance) {
        java.math.BigInteger zero_count = java.math.BigInteger.valueOf(0);
        if ((double)(inductance) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(frequency) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(reactance) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if (zero_count.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if ((double)(inductance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Inductance cannot be negative"));
        }
        if ((double)(frequency) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Frequency cannot be negative"));
        }
        if ((double)(reactance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Inductive reactance cannot be negative"));
        }
        if ((double)(inductance) == (double)(0.0)) {
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("inductance", (double)((double)(reactance) / (double)(((double)((double)(2.0) * (double)(PI)) * (double)(frequency))))); }}));
        }
        if ((double)(frequency) == (double)(0.0)) {
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("frequency", (double)((double)(reactance) / (double)(((double)((double)(2.0) * (double)(PI)) * (double)(inductance))))); }}));
        }
        return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("reactance", (double)((double)((double)((double)(2.0) * (double)(PI)) * (double)(frequency)) * (double)(inductance))); }}));
    }
    public static void main(String[] args) {
        System.out.println(ind_reactance((double)(0.0), (double)(10000.0), (double)(50.0)));
        System.out.println(ind_reactance((double)(0.035), (double)(0.0), (double)(50.0)));
        System.out.println(ind_reactance((double)(3.5e-05), (double)(1000.0), (double)(0.0)));
    }
}
