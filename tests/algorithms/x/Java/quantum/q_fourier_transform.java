public class Main {

    static String to_bits(int n, int width) {
        String res = "";
        int num = n;
        int w = width;
        while (w > 0) {
            res = _p(Math.floorMod(num, 2)) + res;
            num = Math.floorDiv(num, 2);
            w = w - 1;
        }
        return res;
    }

    static java.util.Map<String,Integer> quantum_fourier_transform(int number_of_qubits) {
        if (number_of_qubits <= 0) {
            throw new RuntimeException(String.valueOf("number of qubits must be > 0."));
        }
        if (number_of_qubits > 10) {
            throw new RuntimeException(String.valueOf("number of qubits too large to simulate(>10)."));
        }
        int shots = 10000;
        int states = 1;
        int p = 0;
        while (p < number_of_qubits) {
            states = states * 2;
            p = p + 1;
        }
        int per_state = Math.floorDiv(shots, states);
        java.util.Map<String,Integer> counts = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i = 0;
        while (i < states) {
counts.put(to_bits(i, number_of_qubits), per_state);
            i = i + 1;
        }
        return counts;
    }
    public static void main(String[] args) {
        System.out.println("Total count for quantum fourier transform state is: " + _p(quantum_fourier_transform(3)));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
