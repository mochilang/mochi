public class Main {
    static double G;

    static double potential_energy(double mass, double height) {
        if (mass < 0.0) {
            throw new RuntimeException(String.valueOf("The mass of a body cannot be negative"));
        }
        if (height < 0.0) {
            throw new RuntimeException(String.valueOf("The height above the ground cannot be negative"));
        }
        return mass * G * height;
    }
    public static void main(String[] args) {
        G = 9.80665;
        System.out.println(potential_energy(10.0, 10.0));
        System.out.println(potential_energy(10.0, 5.0));
        System.out.println(potential_energy(2.0, 8.0));
    }
}
