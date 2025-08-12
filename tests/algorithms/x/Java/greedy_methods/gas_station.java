public class Main {
    static class GasStation {
        long gas_quantity;
        long cost;
        GasStation(long gas_quantity, long cost) {
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

    static GasStation[] get_gas_stations(long[] gas_quantities, long[] costs) {
        GasStation[] stations = ((GasStation[])(new GasStation[]{}));
        long i_1 = 0L;
        while (i_1 < gas_quantities.length) {
            stations = ((GasStation[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stations), java.util.stream.Stream.of(new GasStation(gas_quantities[(int)((long)(i_1))], costs[(int)((long)(i_1))]))).toArray(GasStation[]::new)));
            i_1 = i_1 + 1;
        }
        return stations;
    }

    static long can_complete_journey(GasStation[] gas_stations) {
        long total_gas = 0L;
        long total_cost_1 = 0L;
        long i_3 = 0L;
        while (i_3 < gas_stations.length) {
            total_gas = total_gas + ((Number)(((Object)(((java.util.Map)gas_stations[(int)((long)(i_3))])).get("gas_quantity")))).intValue();
            total_cost_1 = total_cost_1 + ((Number)(((Object)(((java.util.Map)gas_stations[(int)((long)(i_3))])).get("cost")))).intValue();
            i_3 = i_3 + 1;
        }
        if (total_gas < total_cost_1) {
            return -1;
        }
        long start_1 = 0L;
        long net_1 = 0L;
        i_3 = 0;
        while (i_3 < gas_stations.length) {
            GasStation station_1 = gas_stations[(int)((long)(i_3))];
            net_1 = net_1 + (long)(((long)(((java.util.Map)station_1)).getOrDefault("gas_quantity", 0L))) - (long)(((long)(((java.util.Map)station_1)).getOrDefault("cost", 0L)));
            if (net_1 < 0) {
                start_1 = i_3 + 1;
                net_1 = 0;
            }
            i_3 = i_3 + 1;
        }
        return start_1;
    }
    public static void main(String[] args) {
        example1 = ((GasStation[])(get_gas_stations(((long[])(new long[]{1, 2, 3, 4, 5})), ((long[])(new long[]{3, 4, 5, 1, 2})))));
        System.out.println(_p(can_complete_journey(((GasStation[])(example1)))));
        example2 = ((GasStation[])(get_gas_stations(((long[])(new long[]{2, 3, 4})), ((long[])(new long[]{3, 4, 3})))));
        System.out.println(_p(can_complete_journey(((GasStation[])(example2)))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
