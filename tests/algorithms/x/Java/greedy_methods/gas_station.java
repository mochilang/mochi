public class Main {
    static class GasStation {
        int gas_quantity;
        int cost;
        GasStation(int gas_quantity, int cost) {
            this.gas_quantity = gas_quantity;
            this.cost = cost;
        }
        GasStation() {}
        @Override public String toString() {
            return String.format("{'gas_quantity': %s, 'cost': %s}", String.valueOf(gas_quantity), String.valueOf(cost));
        }
    }

    static GasStation[] example1;
    static GasStation[] example2;

    static GasStation[] get_gas_stations(int[] gas_quantities, int[] costs) {
        GasStation[] stations = ((GasStation[])(new GasStation[]{}));
        int i = 0;
        while (i < gas_quantities.length) {
            stations = ((GasStation[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stations), java.util.stream.Stream.of(new GasStation(gas_quantities[i], costs[i]))).toArray(GasStation[]::new)));
            i = i + 1;
        }
        return stations;
    }

    static int can_complete_journey(GasStation[] gas_stations) {
        int total_gas = 0;
        int total_cost = 0;
        int i_1 = 0;
        while (i_1 < gas_stations.length) {
            total_gas = total_gas + ((Number)(((Object)(((java.util.Map)gas_stations[i_1])).get("gas_quantity")))).intValue();
            total_cost = total_cost + ((Number)(((Object)(((java.util.Map)gas_stations[i_1])).get("cost")))).intValue();
            i_1 = i_1 + 1;
        }
        if (total_gas < total_cost) {
            return -1;
        }
        int start = 0;
        int net = 0;
        i_1 = 0;
        while (i_1 < gas_stations.length) {
            GasStation station = gas_stations[i_1];
            net = net + (int)(((int)(((java.util.Map)station)).getOrDefault("gas_quantity", 0))) - (int)(((int)(((java.util.Map)station)).getOrDefault("cost", 0)));
            if (net < 0) {
                start = i_1 + 1;
                net = 0;
            }
            i_1 = i_1 + 1;
        }
        return start;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            example1 = ((GasStation[])(get_gas_stations(((int[])(new int[]{1, 2, 3, 4, 5})), ((int[])(new int[]{3, 4, 5, 1, 2})))));
            System.out.println(_p(can_complete_journey(((GasStation[])(example1)))));
            example2 = ((GasStation[])(get_gas_stations(((int[])(new int[]{2, 3, 4})), ((int[])(new int[]{3, 4, 3})))));
            System.out.println(_p(can_complete_journey(((GasStation[])(example2)))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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
