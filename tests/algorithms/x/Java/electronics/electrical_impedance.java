public class Main {

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return (double)(0.0);
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(guess_1);
    }

    static java.util.Map<String,Double> electrical_impedance(double resistance, double reactance, double impedance) {
        java.math.BigInteger zero_count = java.math.BigInteger.valueOf(0);
        if ((double)(resistance) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(reactance) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if ((double)(impedance) == (double)(0.0)) {
            zero_count = new java.math.BigInteger(String.valueOf(zero_count.add(java.math.BigInteger.valueOf(1))));
        }
        if (zero_count.compareTo(java.math.BigInteger.valueOf(1)) != 0) {
            throw new RuntimeException(String.valueOf("One and only one argument must be 0"));
        }
        if ((double)(resistance) == (double)(0.0)) {
            double value_3 = (double)(sqrtApprox((double)((double)((double)(impedance) * (double)(impedance)) - (double)((double)(reactance) * (double)(reactance)))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("resistance", (double)(value_3)); }}));
        } else         if ((double)(reactance) == (double)(0.0)) {
            double value_4 = (double)(sqrtApprox((double)((double)((double)(impedance) * (double)(impedance)) - (double)((double)(resistance) * (double)(resistance)))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("reactance", (double)(value_4)); }}));
        } else         if ((double)(impedance) == (double)(0.0)) {
            double value_5 = (double)(sqrtApprox((double)((double)((double)(resistance) * (double)(resistance)) + (double)((double)(reactance) * (double)(reactance)))));
            return ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>() {{ put("impedance", (double)(value_5)); }}));
        } else {
            throw new RuntimeException(String.valueOf("Exactly one argument must be 0"));
        }
    }
    public static void main(String[] args) {
        System.out.println(electrical_impedance((double)(3.0), (double)(4.0), (double)(0.0)));
        System.out.println(electrical_impedance((double)(0.0), (double)(4.0), (double)(5.0)));
        System.out.println(electrical_impedance((double)(3.0), (double)(0.0), (double)(5.0)));
    }
}
