public class Main {

    static double expApprox(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < (double)(0.0)) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        java.math.BigInteger n_1 = java.math.BigInteger.valueOf(1);
        while (n_1.compareTo(java.math.BigInteger.valueOf(30)) < 0) {
            term_1 = (double)((double)((double)(term_1) * (double)(y)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = new java.math.BigInteger(String.valueOf(n_1.add(java.math.BigInteger.valueOf(1))));
        }
        if (is_neg_1) {
            return (double)((double)(1.0) / (double)(sum_1));
        }
        return (double)(sum_1);
    }

    static double round3(double x) {
        double scaled = (double)((double)(x) * (double)(1000.0));
        if ((double)(scaled) >= (double)(0.0)) {
            scaled = (double)((double)(scaled) + (double)(0.5));
        } else {
            scaled = (double)((double)(scaled) - (double)(0.5));
        }
        java.math.BigInteger scaled_int_1 = new java.math.BigInteger(String.valueOf(((Number)(scaled)).intValue()));
        return (double)((double)((((Number)(scaled_int_1)).doubleValue())) / (double)(1000.0));
    }

    static double charging_capacitor(double source_voltage, double resistance, double capacitance, double time_sec) {
        if ((double)(source_voltage) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Source voltage must be positive."));
        }
        if ((double)(resistance) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Resistance must be positive."));
        }
        if ((double)(capacitance) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Capacitance must be positive."));
        }
        double exponent_1 = (double)((double)(-time_sec) / (double)(((double)(resistance) * (double)(capacitance))));
        double voltage_1 = (double)((double)(source_voltage) * (double)(((double)(1.0) - (double)(expApprox((double)(exponent_1))))));
        return (double)(round3((double)(voltage_1)));
    }
    public static void main(String[] args) {
        System.out.println(charging_capacitor((double)(0.2), (double)(0.9), (double)(8.4), (double)(0.5)));
        System.out.println(charging_capacitor((double)(2.2), (double)(3.5), (double)(2.4), (double)(9.0)));
        System.out.println(charging_capacitor((double)(15.0), (double)(200.0), (double)(20.0), (double)(2.0)));
        System.out.println(charging_capacitor((double)(20.0), (double)(2000.0), (double)(0.0003), (double)(4.0)));
    }
}
