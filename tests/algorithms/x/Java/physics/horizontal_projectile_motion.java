public class Main {
    static double PI;
    static double TWO_PI;
    static double g;
    static double v0;
    static double angle;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double sin(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2 = y * y;
        double y3 = y2 * y;
        double y5 = y3 * y2;
        double y7 = y5 * y2;
        return y - y3 / 6.0 + y5 / 120.0 - y7 / 5040.0;
    }

    static double deg_to_rad(double deg) {
        return deg * PI / 180.0;
    }

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow10(int n) {
        double result = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            result = result * 10.0;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        double y_1 = floor(x * m + 0.5);
        return y_1 / m;
    }

    static void check_args(double init_velocity, double angle) {
        if (angle > 90.0 || angle < 1.0) {
            throw new RuntimeException(String.valueOf("Invalid angle. Range is 1-90 degrees."));
        }
        if (init_velocity < 0.0) {
            throw new RuntimeException(String.valueOf("Invalid velocity. Should be a positive number."));
        }
    }

    static double horizontal_distance(double init_velocity, double angle) {
        check_args(init_velocity, angle);
        double radians = deg_to_rad(2.0 * angle);
        return round((init_velocity * init_velocity * sin(radians)) / g, 2);
    }

    static double max_height(double init_velocity, double angle) {
        check_args(init_velocity, angle);
        double radians_1 = deg_to_rad(angle);
        double s = sin(radians_1);
        return round((init_velocity * init_velocity * s * s) / (2.0 * g), 2);
    }

    static double total_time(double init_velocity, double angle) {
        check_args(init_velocity, angle);
        double radians_2 = deg_to_rad(angle);
        return round((2.0 * init_velocity * sin(radians_2)) / g, 2);
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        TWO_PI = 6.283185307179586;
        g = 9.80665;
        v0 = 25.0;
        angle = 20.0;
        System.out.println(horizontal_distance(v0, angle));
        System.out.println(max_height(v0, angle));
        System.out.println(total_time(v0, angle));
    }
}
