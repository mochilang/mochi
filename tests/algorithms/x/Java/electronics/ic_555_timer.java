public class Main {

    static double astable_frequency(double resistance_1, double resistance_2, double capacitance) {
        if ((double)(resistance_1) <= (double)(0.0) || (double)(resistance_2) <= (double)(0.0) || (double)(capacitance) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All values must be positive"));
        }
        return (double)((double)(((double)(1.44) / (double)(((double)(((double)(resistance_1) + (double)((double)(2.0) * (double)(resistance_2)))) * (double)(capacitance))))) * (double)(1000000.0));
    }

    static double astable_duty_cycle(double resistance_1, double resistance_2) {
        if ((double)(resistance_1) <= (double)(0.0) || (double)(resistance_2) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("All values must be positive"));
        }
        return (double)((double)((double)(((double)(resistance_1) + (double)(resistance_2))) / (double)(((double)(resistance_1) + (double)((double)(2.0) * (double)(resistance_2))))) * (double)(100.0));
    }
    public static void main(String[] args) {
        System.out.println(astable_frequency((double)(45.0), (double)(45.0), (double)(7.0)));
        System.out.println(astable_duty_cycle((double)(45.0), (double)(45.0)));
    }
}
