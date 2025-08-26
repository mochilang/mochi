public class Main {
    static double UNIVERSAL_GAS_CONSTANT = (double)(8.314462);

    static double pressure_of_gas_system(double moles, double kelvin, double volume) {
        if ((double)(moles) < ((java.math.BigInteger)(0)).doubleValue() || (double)(kelvin) < ((java.math.BigInteger)(0)).doubleValue() || (double)(volume) < ((java.math.BigInteger)(0)).doubleValue()) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)((double)((double)((double)(moles) * (double)(kelvin)) * (double)(UNIVERSAL_GAS_CONSTANT)) / (double)(volume));
    }

    static double volume_of_gas_system(double moles, double kelvin, double pressure) {
        if ((double)(moles) < ((java.math.BigInteger)(0)).doubleValue() || (double)(kelvin) < ((java.math.BigInteger)(0)).doubleValue() || (double)(pressure) < ((java.math.BigInteger)(0)).doubleValue()) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)((double)((double)((double)(moles) * (double)(kelvin)) * (double)(UNIVERSAL_GAS_CONSTANT)) / (double)(pressure));
    }

    static double temperature_of_gas_system(double moles, double volume, double pressure) {
        if ((double)(moles) < ((java.math.BigInteger)(0)).doubleValue() || (double)(volume) < ((java.math.BigInteger)(0)).doubleValue() || (double)(pressure) < ((java.math.BigInteger)(0)).doubleValue()) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)((double)((double)(pressure) * (double)(volume)) / (double)(((double)(moles) * (double)(UNIVERSAL_GAS_CONSTANT))));
    }

    static double moles_of_gas_system(double kelvin, double volume, double pressure) {
        if ((double)(kelvin) < ((java.math.BigInteger)(0)).doubleValue() || (double)(volume) < ((java.math.BigInteger)(0)).doubleValue() || (double)(pressure) < ((java.math.BigInteger)(0)).doubleValue()) {
            throw new RuntimeException(String.valueOf("Invalid inputs. Enter positive value."));
        }
        return (double)((double)((double)(pressure) * (double)(volume)) / (double)(((double)(kelvin) * (double)(UNIVERSAL_GAS_CONSTANT))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(pressure_of_gas_system((double)(2.0), (double)(100.0), (double)(5.0)));
            System.out.println(volume_of_gas_system((double)(0.5), (double)(273.0), (double)(0.004)));
            System.out.println(temperature_of_gas_system((double)(2.0), (double)(100.0), (double)(5.0)));
            System.out.println(moles_of_gas_system((double)(100.0), (double)(5.0), (double)(10.0)));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
