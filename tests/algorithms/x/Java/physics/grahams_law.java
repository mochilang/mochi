public class Main {

    static double to_float(long x) {
        return (double)(x) * (double)(1.0);
    }

    static double round6(double x) {
        double factor = (double)(1000000.0);
        return (double)(((Number)(((Number)((double)((double)(x) * (double)(factor)) + (double)(0.5))).intValue())).doubleValue()) / (double)(factor);
    }

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess;
    }

    static boolean validate(double[] values) {
        if ((long)(values.length) == 0L) {
            return false;
        }
        long i_3 = 0L;
        while ((long)(i_3) < (long)(values.length)) {
            if ((double)(values[(int)((long)(i_3))]) <= (double)(0.0)) {
                return false;
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return true;
    }

    static double effusion_ratio(double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{m1, m2})))) {
            System.out.println("ValueError: Molar mass values must greater than 0.");
            return 0.0;
        }
        return round6((double)(sqrtApprox((double)((double)(m2) / (double)(m1)))));
    }

    static double first_effusion_rate(double rate, double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{rate, m1, m2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        return round6((double)((double)(rate) * (double)(sqrtApprox((double)((double)(m2) / (double)(m1))))));
    }

    static double second_effusion_rate(double rate, double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{rate, m1, m2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        return round6((double)((double)(rate) / (double)(sqrtApprox((double)((double)(m2) / (double)(m1))))));
    }

    static double first_molar_mass(double mass, double r1, double r2) {
        if (!(Boolean)validate(((double[])(new double[]{mass, r1, r2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        double ratio_1 = (double)((double)(r1) / (double)(r2));
        return round6((double)((double)(mass) / (double)(((double)(ratio_1) * (double)(ratio_1)))));
    }

    static double second_molar_mass(double mass, double r1, double r2) {
        if (!(Boolean)validate(((double[])(new double[]{mass, r1, r2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        double ratio_3 = (double)((double)(r1) / (double)(r2));
        return round6((double)((double)(((double)(ratio_3) * (double)(ratio_3))) / (double)(mass)));
    }
    public static void main(String[] args) {
        System.out.println(effusion_ratio((double)(2.016), (double)(4.002)));
        System.out.println(first_effusion_rate((double)(1.0), (double)(2.016), (double)(4.002)));
        System.out.println(second_effusion_rate((double)(1.0), (double)(2.016), (double)(4.002)));
        System.out.println(first_molar_mass((double)(2.0), (double)(1.408943), (double)(0.709752)));
        System.out.println(second_molar_mass((double)(2.0), (double)(1.408943), (double)(0.709752)));
    }
}
