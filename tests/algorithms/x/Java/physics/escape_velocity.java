public class Main {

    static double pow10(int n) {
        double p = 1.0;
        int k = 0;
        if (n >= 0) {
            while (k < n) {
                p = p * 10.0;
                k = k + 1;
            }
        } else {
            int m = -n;
            while (k < m) {
                p = p / 10.0;
                k = k + 1;
            }
        }
        return p;
    }

    static double sqrt_newton(double n) {
        if (n == 0.0) {
            return 0.0;
        }
        double x = n;
        int j = 0;
        while (j < 20) {
            x = (x + n / x) / 2.0;
            j = j + 1;
        }
        return x;
    }

    static double round3(double x) {
        double y = x * 1000.0 + 0.5;
        int yi = ((Number)(y)).intValue();
        if ((((Number)(yi)).doubleValue()) > y) {
            yi = yi - 1;
        }
        return (((Number)(yi)).doubleValue()) / 1000.0;
    }

    static double escape_velocity(double mass, double radius) {
        if (radius == 0.0) {
            throw new RuntimeException(String.valueOf("Radius cannot be zero."));
        }
        double G = 6.6743 * pow10(-11);
        double velocity = sqrt_newton(2.0 * G * mass / radius);
        return round3(velocity);
    }
    public static void main(String[] args) {
        System.out.println(escape_velocity(5.972 * pow10(24), 6.371 * pow10(6)));
        System.out.println(escape_velocity(7.348 * pow10(22), 1.737 * pow10(6)));
        System.out.println(escape_velocity(1.898 * pow10(27), 6.9911 * pow10(7)));
    }
}
