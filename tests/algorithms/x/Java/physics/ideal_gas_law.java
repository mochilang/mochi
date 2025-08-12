public class Main {
    static double UNIVERSAL_GAS_CONSTANT = 8.314462;

    static double pressure_of_gas_system(double moles, double kelvin, double volume) {
        if ((double)(moles) < (double)(0) || (double)(kelvin) < (double)(0) || (double)(volume) < (double)(0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)(moles) * (double)(kelvin) * (double)(UNIVERSAL_GAS_CONSTANT) / (double)(volume);
    }

    static double volume_of_gas_system(double moles, double kelvin, double pressure) {
        if ((double)(moles) < (double)(0) || (double)(kelvin) < (double)(0) || (double)(pressure) < (double)(0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)(moles) * (double)(kelvin) * (double)(UNIVERSAL_GAS_CONSTANT) / (double)(pressure);
    }

    static double temperature_of_gas_system(double moles, double volume, double pressure) {
        if ((double)(moles) < (double)(0) || (double)(volume) < (double)(0) || (double)(pressure) < (double)(0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)(pressure) * (double)(volume) / ((double)(moles) * (double)(UNIVERSAL_GAS_CONSTANT));
    }

    static double moles_of_gas_system(double kelvin, double volume, double pressure) {
        if ((double)(kelvin) < (double)(0) || (double)(volume) < (double)(0) || (double)(pressure) < (double)(0)) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)(pressure) * (double)(volume) / ((double)(kelvin) * (double)(UNIVERSAL_GAS_CONSTANT));
    }
    public static void main(String[] args) {
        System.out.println(pressure_of_gas_system(2.0, 100.0, 5.0));
        System.out.println(volume_of_gas_system(0.5, 273.0, 0.004));
        System.out.println(temperature_of_gas_system(2.0, 100.0, 5.0));
        System.out.println(moles_of_gas_system(100.0, 5.0, 10.0));
    }
}
