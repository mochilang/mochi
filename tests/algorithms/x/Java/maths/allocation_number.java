public class Main {

    static String[] allocation_num(long number_of_bytes, long partitions) {
        if (partitions <= 0) {
            throw new RuntimeException(String.valueOf("partitions must be a positive number!"));
        }
        if (partitions > number_of_bytes) {
            throw new RuntimeException(String.valueOf("partitions can not > number_of_bytes!"));
        }
        long bytes_per_partition_1 = Math.floorDiv(number_of_bytes, partitions);
        String[] allocation_list_1 = ((String[])(new String[]{}));
        long i_1 = 0;
        while (i_1 < partitions) {
            long start_bytes_1 = i_1 * bytes_per_partition_1 + 1;
            long end_bytes_1 = i_1 == partitions - 1 ? number_of_bytes : (i_1 + 1) * bytes_per_partition_1;
            allocation_list_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(allocation_list_1), java.util.stream.Stream.of(_p(start_bytes_1) + "-" + _p(end_bytes_1))).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        return allocation_list_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(allocation_num(16647, 4)));
        System.out.println(_p(allocation_num(50000, 5)));
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
