public class Main {

    static double expApprox(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)((double)(1.0) / (double)(expApprox((double)(-x))));
        }
        if ((double)(x) > (double)(1.0)) {
            double half_1 = (double)(expApprox((double)((double)(x) / (double)(2.0))));
            return (double)((double)(half_1) * (double)(half_1));
        }
        double sum_1 = (double)(1.0);
        double term_1 = (double)(1.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            term_1 = (double)((double)((double)(term_1) * (double)(x)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(sum_1);
    }

    static double floor(double x) {
        java.math.BigInteger i = new java.math.BigInteger(String.valueOf(((Number)(x)).intValue()));
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = new java.math.BigInteger(String.valueOf(i.subtract(java.math.BigInteger.valueOf(1))));
        }
        return (double)(((Number)(i)).doubleValue());
    }

    static double pow10(java.math.BigInteger n) {
        double result = (double)(1.0);
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(n) < 0) {
            result = (double)((double)(result) * (double)(10.0));
            i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(result);
    }

    static double round(double x, java.math.BigInteger n) {
        double m = (double)(pow10(new java.math.BigInteger(String.valueOf(n))));
        return (double)(Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m));
    }

    static double charging_inductor(double source_voltage, double resistance, double inductance, double time) {
        if ((double)(source_voltage) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Source voltage must be positive."));
        }
        if ((double)(resistance) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Resistance must be positive."));
        }
        if ((double)(inductance) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Inductance must be positive."));
        }
        double exponent_1 = (double)((double)(((double)(-time) * (double)(resistance))) / (double)(inductance));
        double current_1 = (double)((double)((double)(source_voltage) / (double)(resistance)) * (double)(((double)(1.0) - (double)(expApprox((double)(exponent_1))))));
        return (double)(round((double)(current_1), java.math.BigInteger.valueOf(3)));
    }
    public static void main(String[] args) {
        System.out.println(charging_inductor((double)(5.8), (double)(1.5), (double)(2.3), (double)(2.0)));
        System.out.println(charging_inductor((double)(8.0), (double)(5.0), (double)(3.0), (double)(2.0)));
        System.out.println(charging_inductor((double)(8.0), (double)((double)(5.0) * (double)(pow10(java.math.BigInteger.valueOf(2)))), (double)(3.0), (double)(2.0)));
    }
}
