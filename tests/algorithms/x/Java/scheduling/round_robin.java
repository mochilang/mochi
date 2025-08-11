public class Main {

    static long[] calculate_waiting_times(long[] burst_times) {
        long quantum = 2;
        long[] rem_1 = ((long[])(new long[]{}));
        long i_1 = 0;
        while (i_1 < burst_times.length) {
            rem_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(rem_1), java.util.stream.LongStream.of(burst_times[(int)(i_1)])).toArray()));
            i_1 = i_1 + 1;
        }
        long[] waiting_1 = ((long[])(new long[]{}));
        i_1 = 0;
        while (i_1 < burst_times.length) {
            waiting_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(waiting_1), java.util.stream.LongStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        long t_1 = 0;
        while (true) {
            boolean done_1 = true;
            long j_1 = 0;
            while (j_1 < burst_times.length) {
                if (rem_1[(int)(j_1)] > 0) {
                    done_1 = false;
                    if (rem_1[(int)(j_1)] > quantum) {
                        t_1 = t_1 + quantum;
rem_1[(int)(j_1)] = rem_1[(int)(j_1)] - quantum;
                    } else {
                        t_1 = t_1 + rem_1[(int)(j_1)];
waiting_1[(int)(j_1)] = t_1 - burst_times[(int)(j_1)];
rem_1[(int)(j_1)] = 0;
                    }
                }
                j_1 = j_1 + 1;
            }
            if (done_1) {
                return waiting_1;
            }
        }
    }

    static long[] calculate_turn_around_times(long[] burst_times, long[] waiting_times) {
        long[] result = ((long[])(new long[]{}));
        long i_3 = 0;
        while (i_3 < burst_times.length) {
            result = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(result), java.util.stream.LongStream.of(burst_times[(int)(i_3)] + waiting_times[(int)(i_3)])).toArray()));
            i_3 = i_3 + 1;
        }
        return result;
    }

    static double mean(long[] values) {
        long total = 0;
        long i_5 = 0;
        while (i_5 < values.length) {
            total = total + values[(int)(i_5)];
            i_5 = i_5 + 1;
        }
        return (((Number)(total)).doubleValue()) / (((Number)(values.length)).doubleValue());
    }

    static String format_float_5(double x) {
        long scaled = ((Number)(x * 100000.0 + 0.5)).intValue();
        long int_part_1 = Math.floorDiv(scaled, 100000);
        long frac_part_1 = Math.floorMod(scaled, 100000);
        String frac_str_1 = _p(frac_part_1);
        while (_runeLen(frac_str_1) < 5) {
            frac_str_1 = "0" + frac_str_1;
        }
        return _p(int_part_1) + "." + frac_str_1;
    }

    static void main() {
        long[] burst_times = ((long[])(new long[]{3, 5, 7}));
        long[] waiting_times_1 = ((long[])(calculate_waiting_times(((long[])(burst_times)))));
        long[] turn_around_times_1 = ((long[])(calculate_turn_around_times(((long[])(burst_times)), ((long[])(waiting_times_1)))));
        System.out.println("Process ID \tBurst Time \tWaiting Time \tTurnaround Time");
        long i_7 = 0;
        while (i_7 < burst_times.length) {
            String line_1 = "  " + _p(i_7 + 1) + "\t\t  " + _p(_geti(burst_times, i_7)) + "\t\t  " + _p(_geti(waiting_times_1, i_7)) + "\t\t  " + _p(_geti(turn_around_times_1, i_7));
            System.out.println(line_1);
            i_7 = i_7 + 1;
        }
        System.out.println("");
        System.out.println("Average waiting time = " + String.valueOf(format_float_5(mean(((long[])(waiting_times_1))))));
        System.out.println("Average turn around time = " + String.valueOf(format_float_5(mean(((long[])(turn_around_times_1))))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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

    static Long _geti(long[] a, long i) {
        return (i >= 0 && i < a.length) ? a[(int)i] : null;
    }
}
