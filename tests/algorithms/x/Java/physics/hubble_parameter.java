public class Main {

    static double pow(double base, int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double sqrt_approx(double x) {
        if (x == 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i_1 = 0;
        while (i_1 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess;
    }

    static double hubble_parameter(double hubble_constant, double radiation_density, double matter_density, double dark_energy, double redshift) {
        double[] parameters = ((double[])(new double[]{redshift, radiation_density, matter_density, dark_energy}));
        int i_2 = 0;
        while (i_2 < parameters.length) {
            if (parameters[i_2] < 0.0) {
                throw new RuntimeException(String.valueOf("All input parameters must be positive"));
            }
            i_2 = i_2 + 1;
        }
        i_2 = 1;
        while (i_2 < 4) {
            if (parameters[i_2] > 1.0) {
                throw new RuntimeException(String.valueOf("Relative densities cannot be greater than one"));
            }
            i_2 = i_2 + 1;
        }
        double curvature = 1.0 - (matter_density + radiation_density + dark_energy);
        double zp1 = redshift + 1.0;
        double e2 = radiation_density * pow(zp1, 4) + matter_density * pow(zp1, 3) + curvature * pow(zp1, 2) + dark_energy;
        return hubble_constant * sqrt_approx(e2);
    }

    static void test_hubble_parameter() {
        double h = hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0.0);
        if (h < 68.2999 || h > 68.3001) {
            throw new RuntimeException(String.valueOf("hubble_parameter test failed"));
        }
    }

    static void main() {
        test_hubble_parameter();
        System.out.println(hubble_parameter(68.3, 0.0001, 0.3, 0.7, 0.0));
    }
    public static void main(String[] args) {
        main();
    }
}
