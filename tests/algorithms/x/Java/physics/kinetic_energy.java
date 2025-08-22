public class Main {

    static double kinetic_energy(double mass, double velocity) {
        if ((double)(mass) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("The mass of a body cannot be negative"));
        }
        double v_1 = (double)(velocity);
        if ((double)(v_1) < (double)(0.0)) {
            v_1 = (double)(-v_1);
        }
        return (double)((double)((double)(0.5) * (double)(mass)) * (double)(v_1)) * (double)(v_1);
    }
    public static void main(String[] args) {
        System.out.println(kinetic_energy((double)(10.0), (double)(10.0)));
        System.out.println(kinetic_energy((double)(0.0), (double)(10.0)));
        System.out.println(kinetic_energy((double)(10.0), (double)(0.0)));
        System.out.println(kinetic_energy((double)(20.0), (double)(-20.0)));
        System.out.println(kinetic_energy((double)(0.0), (double)(0.0)));
        System.out.println(kinetic_energy((double)(2.0), (double)(2.0)));
        System.out.println(kinetic_energy((double)(100.0), (double)(100.0)));
    }
}
