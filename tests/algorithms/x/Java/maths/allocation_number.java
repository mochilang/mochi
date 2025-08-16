public class Main {

    static String[] allocation_num(long number_of_bytes, long partitions) {
        if ((long)(partitions) <= 0L) {
            throw new RuntimeException(String.valueOf("partitions must be a positive number!"));
        }
        if ((long)(partitions) > (long)(number_of_bytes)) {
            throw new RuntimeException(String.valueOf("partitions can not > number_of_bytes!"));
        }
        long bytes_per_partition_1 = Math.floorDiv(((long)(number_of_bytes)), ((long)(partitions)));
        String[] allocation_list_1 = ((String[])(new String[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(partitions)) {
            long start_bytes_1 = (long)((long)((long)(i_1) * (long)(bytes_per_partition_1)) + 1L);
            long end_bytes_1 = (long)((long)(i_1) == (long)((long)(partitions) - 1L) ? number_of_bytes : (long)(((long)(i_1) + 1L)) * (long)(bytes_per_partition_1));
            allocation_list_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(allocation_list_1), java.util.stream.Stream.of(_p(start_bytes_1) + "-" + _p(end_bytes_1))).toArray(String[]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return allocation_list_1;
    }
    public static void main(String[] args) {
        System.out.println(_p(allocation_num(16647L, 4L)));
        System.out.println(_p(allocation_num(50000L, 5L)));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
