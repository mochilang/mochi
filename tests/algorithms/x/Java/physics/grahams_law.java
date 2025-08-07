public class Main {

    static double to_float(int x) {
        return x * 1.0;
    }

    static double round6(double x) {
        double factor = 1000000.0;
        return to_float(((Number)(x * factor + 0.5)).intValue()) / factor;
    }

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static boolean validate(double[] values) {
        if (values.length == 0) {
            return false;
        }
        int i_1 = 0;
        while (i_1 < values.length) {
            if (values[i_1] <= 0.0) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static double effusion_ratio(double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{m1, m2})))) {
            System.out.println("ValueError: Molar mass values must greater than 0.");
            return 0.0;
        }
        return round6(sqrtApprox(m2 / m1));
    }

    static double first_effusion_rate(double rate, double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{rate, m1, m2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        return round6(rate * sqrtApprox(m2 / m1));
    }

    static double second_effusion_rate(double rate, double m1, double m2) {
        if (!(Boolean)validate(((double[])(new double[]{rate, m1, m2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        return round6(rate / sqrtApprox(m2 / m1));
    }

    static double first_molar_mass(double mass, double r1, double r2) {
        if (!(Boolean)validate(((double[])(new double[]{mass, r1, r2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        double ratio = r1 / r2;
        return round6(mass / (ratio * ratio));
    }

    static double second_molar_mass(double mass, double r1, double r2) {
        if (!(Boolean)validate(((double[])(new double[]{mass, r1, r2})))) {
            System.out.println("ValueError: Molar mass and effusion rate values must greater than 0.");
            return 0.0;
        }
        double ratio_1 = r1 / r2;
        return round6((ratio_1 * ratio_1) / mass);
    }
    public static void main(String[] args) {
        System.out.println(effusion_ratio(2.016, 4.002));
        System.out.println(first_effusion_rate(1.0, 2.016, 4.002));
        System.out.println(second_effusion_rate(1.0, 2.016, 4.002));
        System.out.println(first_molar_mass(2.0, 1.408943, 0.709752));
        System.out.println(second_molar_mass(2.0, 1.408943, 0.709752));
    }
}
