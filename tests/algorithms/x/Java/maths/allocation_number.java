public class Main {

    static String[] allocation_num(int number_of_bytes, int partitions) {
        if (partitions <= 0) {
            throw new RuntimeException(String.valueOf("partitions must be a positive number!"));
        }
        if (partitions > number_of_bytes) {
            throw new RuntimeException(String.valueOf("partitions can not > number_of_bytes!"));
        }
        int bytes_per_partition = number_of_bytes / partitions;
        String[] allocation_list = ((String[])(new String[]{}));
        int i = 0;
        while (i < partitions) {
            int start_bytes = i * bytes_per_partition + 1;
            int end_bytes = i == partitions - 1 ? number_of_bytes : (i + 1) * bytes_per_partition;
            allocation_list = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(allocation_list), java.util.stream.Stream.of(_p(start_bytes) + "-" + _p(end_bytes))).toArray(String[]::new)));
            i = i + 1;
        }
        return allocation_list;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(allocation_num(16647, 4)));
            System.out.println(_p(allocation_num(50000, 5)));
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
