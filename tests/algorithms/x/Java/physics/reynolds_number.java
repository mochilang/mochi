public class Main {

    static double fabs(double x) {
        if (x < 0.0) {
            return -x;
        } else {
            return x;
        }
    }

    static double reynolds_number(double density, double velocity, double diameter, double viscosity) {
        if (density <= 0.0 || diameter <= 0.0 || viscosity <= 0.0) {
            throw new RuntimeException(String.valueOf("please ensure that density, diameter and viscosity are positive"));
        }
        return (density * fabs(velocity) * diameter) / viscosity;
    }
    public static void main(String[] args) {
        System.out.println(reynolds_number(900.0, 2.5, 0.05, 0.4));
        System.out.println(reynolds_number(450.0, 3.86, 0.078, 0.23));
        System.out.println(reynolds_number(234.0, -4.5, 0.3, 0.44));
    }
}
