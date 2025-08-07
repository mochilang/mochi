public class Main {
    static double UNIVERSAL_GAS_CONSTANT;

    static double pressure_of_gas_system(double moles, double kelvin, double volume) {
        if (moles < 0 || kelvin < 0 || volume < 0) {
            throw_;
            "Invalid inputs. Enter positive value.";
        }
        return moles * kelvin * UNIVERSAL_GAS_CONSTANT / volume;
    }

    static double volume_of_gas_system(double moles, double kelvin, double pressure) {
        if (moles < 0 || kelvin < 0 || pressure < 0) {
            throw_;
            "Invalid inputs. Enter positive value.";
        }
        return moles * kelvin * UNIVERSAL_GAS_CONSTANT / pressure;
    }

    static double temperature_of_gas_system(double moles, double volume, double pressure) {
        if (moles < 0 || volume < 0 || pressure < 0) {
            throw_;
            "Invalid inputs. Enter positive value.";
        }
        return pressure * volume / (moles * UNIVERSAL_GAS_CONSTANT);
    }

    static double moles_of_gas_system(double kelvin, double volume, double pressure) {
        if (kelvin < 0 || volume < 0 || pressure < 0) {
            throw_;
            "Invalid inputs. Enter positive value.";
        }
        return pressure * volume / (kelvin * UNIVERSAL_GAS_CONSTANT);
    }
    public static void main(String[] args) {
        UNIVERSAL_GAS_CONSTANT = 8.314462;
        System.out.println(pressure_of_gas_system(2.0, 100.0, 5.0));
        System.out.println(volume_of_gas_system(0.5, 273.0, 0.004));
        System.out.println(temperature_of_gas_system(2.0, 100.0, 5.0));
        System.out.println(moles_of_gas_system(100.0, 5.0, 10.0));
    }
}
