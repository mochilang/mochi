public class Main {
    static double G = (double)(9.80665);

    static double archimedes_principle(double fluid_density, double volume, double gravity) {
        if ((double)(fluid_density) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Impossible fluid density"));
        }
        if ((double)(volume) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Impossible object volume"));
        }
        if ((double)(gravity) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Impossible gravity"));
        }
        return (double)((double)(fluid_density) * (double)(volume)) * (double)(gravity);
    }

    static double archimedes_principle_default(double fluid_density, double volume) {
        double res = (double)(archimedes_principle((double)(fluid_density), (double)(volume), (double)(G)));
        return res;
    }
    public static void main(String[] args) {
    }
}
