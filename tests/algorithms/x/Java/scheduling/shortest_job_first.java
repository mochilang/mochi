public class Main {

    static int[] calculate_waitingtime(int[] arrival_time, int[] burst_time, int no_of_processes) {
        int[] remaining_time = ((int[])(new int[]{}));
        int i = 0;
        while (i < no_of_processes) {
            remaining_time = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(remaining_time), java.util.stream.IntStream.of(burst_time[i])).toArray()));
            i = i + 1;
        }
        int[] waiting_time = ((int[])(new int[]{}));
        i = 0;
        while (i < no_of_processes) {
            waiting_time = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(waiting_time), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int complete = 0;
        int increment_time = 0;
        int minm = 1000000000;
        int short_ = 0;
        boolean check = false;
        while (complete != no_of_processes) {
            int j = 0;
            while (j < no_of_processes) {
                if (arrival_time[j] <= increment_time && remaining_time[j] > 0 && remaining_time[j] < minm) {
                    minm = remaining_time[j];
                    short_ = j;
                    check = true;
                }
                j = j + 1;
            }
            if (!check) {
                increment_time = increment_time + 1;
                continue;
            }
remaining_time[short_] = remaining_time[short_] - 1;
            minm = remaining_time[short_];
            if (minm == 0) {
                minm = 1000000000;
            }
            if (remaining_time[short_] == 0) {
                complete = complete + 1;
                check = false;
                int finish_time = increment_time + 1;
                int finar = finish_time - arrival_time[short_];
waiting_time[short_] = finar - burst_time[short_];
                if (waiting_time[short_] < 0) {
waiting_time[short_] = 0;
                }
            }
            increment_time = increment_time + 1;
        }
        return waiting_time;
    }

    static int[] calculate_turnaroundtime(int[] burst_time, int no_of_processes, int[] waiting_time) {
        int[] turn_around_time = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < no_of_processes) {
            turn_around_time = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(turn_around_time), java.util.stream.IntStream.of(burst_time[i_1] + waiting_time[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return turn_around_time;
    }

    static double to_float(int x) {
        return x * 1.0;
    }

    static void calculate_average_times(int[] waiting_time, int[] turn_around_time, int no_of_processes) {
        int total_waiting_time = 0;
        int total_turn_around_time = 0;
        int i_2 = 0;
        while (i_2 < no_of_processes) {
            total_waiting_time = total_waiting_time + waiting_time[i_2];
            total_turn_around_time = total_turn_around_time + turn_around_time[i_2];
            i_2 = i_2 + 1;
        }
        double avg_wait = to_float(total_waiting_time) / to_float(no_of_processes);
        double avg_turn = to_float(total_turn_around_time) / to_float(no_of_processes);
        System.out.println("Average waiting time = " + _p(avg_wait));
        System.out.println("Average turn around time = " + _p(avg_turn));
    }
    public static void main(String[] args) {
        System.out.println(calculate_waitingtime(((int[])(new int[]{1, 2, 3, 4})), ((int[])(new int[]{3, 3, 5, 1})), 4));
        System.out.println(calculate_waitingtime(((int[])(new int[]{1, 2, 3})), ((int[])(new int[]{2, 5, 1})), 3));
        System.out.println(calculate_waitingtime(((int[])(new int[]{2, 3})), ((int[])(new int[]{5, 1})), 2));
        System.out.println(calculate_turnaroundtime(((int[])(new int[]{3, 3, 5, 1})), 4, ((int[])(new int[]{0, 3, 5, 0}))));
        System.out.println(calculate_turnaroundtime(((int[])(new int[]{3, 3})), 2, ((int[])(new int[]{0, 3}))));
        System.out.println(calculate_turnaroundtime(((int[])(new int[]{8, 10, 1})), 3, ((int[])(new int[]{1, 0, 3}))));
        calculate_average_times(((int[])(new int[]{0, 3, 5, 0})), ((int[])(new int[]{3, 6, 10, 1})), 4);
        calculate_average_times(((int[])(new int[]{2, 3})), ((int[])(new int[]{3, 6})), 2);
        calculate_average_times(((int[])(new int[]{10, 4, 3})), ((int[])(new int[]{2, 7, 6})), 3);
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
