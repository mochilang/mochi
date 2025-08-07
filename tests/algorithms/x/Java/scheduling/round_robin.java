public class Main {

    static int[] calculate_waiting_times(int[] burst_times) {
        int quantum = 2;
        int[] rem = ((int[])(new int[]{}));
        int i = 0;
        while (i < burst_times.length) {
            rem = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(rem), java.util.stream.IntStream.of(burst_times[i])).toArray()));
            i = i + 1;
        }
        int[] waiting = ((int[])(new int[]{}));
        i = 0;
        while (i < burst_times.length) {
            waiting = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(waiting), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int t = 0;
        while (true) {
            boolean done = true;
            int j = 0;
            while (j < burst_times.length) {
                if (rem[j] > 0) {
                    done = false;
                    if (rem[j] > quantum) {
                        t = t + quantum;
rem[j] = rem[j] - quantum;
                    } else {
                        t = t + rem[j];
waiting[j] = t - burst_times[j];
rem[j] = 0;
                    }
                }
                j = j + 1;
            }
            if (done) {
                return waiting;
            }
        }
        return waiting;
    }

    static int[] calculate_turn_around_times(int[] burst_times, int[] waiting_times) {
        int[] result = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < burst_times.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(burst_times[i_1] + waiting_times[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double mean(int[] values) {
        int total = 0;
        int i_2 = 0;
        while (i_2 < values.length) {
            total = total + values[i_2];
            i_2 = i_2 + 1;
        }
        return (((Number)(total)).doubleValue()) / (((Number)(values.length)).doubleValue());
    }

    static String format_float_5(double x) {
        int scaled = ((Number)(x * 100000.0 + 0.5)).intValue();
        int int_part = Math.floorDiv(scaled, 100000);
        int frac_part = Math.floorMod(scaled, 100000);
        String frac_str = _p(frac_part);
        while (_runeLen(frac_str) < 5) {
            frac_str = "0" + frac_str;
        }
        return _p(int_part) + "." + frac_str;
    }

    static void main() {
        int[] burst_times = ((int[])(new int[]{3, 5, 7}));
        int[] waiting_times = ((int[])(calculate_waiting_times(((int[])(burst_times)))));
        int[] turn_around_times = ((int[])(calculate_turn_around_times(((int[])(burst_times)), ((int[])(waiting_times)))));
        System.out.println("Process ID \tBurst Time \tWaiting Time \tTurnaround Time");
        int i_3 = 0;
        while (i_3 < burst_times.length) {
            String line = "  " + _p(i_3 + 1) + "\t\t  " + _p(_geti(burst_times, i_3)) + "\t\t  " + _p(_geti(waiting_times, i_3)) + "\t\t  " + _p(_geti(turn_around_times, i_3));
            System.out.println(line);
            i_3 = i_3 + 1;
        }
        System.out.println("");
        System.out.println("Average waiting time = " + String.valueOf(format_float_5(mean(((int[])(waiting_times))))));
        System.out.println("Average turn around time = " + String.valueOf(format_float_5(mean(((int[])(turn_around_times))))));
    }
    public static void main(String[] args) {
        main();
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
        return String.valueOf(v);
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
