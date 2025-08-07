public class Main {

    static double kinetic_energy(double mass, double velocity) {
        if (mass < 0.0) {
            throw new RuntimeException(String.valueOf("The mass of a body cannot be negative"));
        }
        double v = velocity;
        if (v < 0.0) {
            v = -v;
        }
        return 0.5 * mass * v * v;
    }
    public static void main(String[] args) {
        System.out.println(kinetic_energy(10.0, 10.0));
        System.out.println(kinetic_energy(0.0, 10.0));
        System.out.println(kinetic_energy(10.0, 0.0));
        System.out.println(kinetic_energy(20.0, -20.0));
        System.out.println(kinetic_energy(0.0, 0.0));
        System.out.println(kinetic_energy(2.0, 2.0));
        System.out.println(kinetic_energy(100.0, 100.0));
    }
}
