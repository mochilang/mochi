public class Main {
    static int no_of_processes;
    static int[] burst_time;
    static int[] arrival_time;
    static int[] waiting_time_1;
    static int[] turn_around_time_1;
    static int i_3 = 0;
    static double avg_wait;
    static double avg_turn;

    static int[] calculate_waitingtime(int[] arrival_time, int[] burst_time, int no_of_processes) {
        int[] waiting_time = new int[0];
        int[] remaining_time = new int[0];
        int i = 0;
        while (i < no_of_processes) {
            waiting_time = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(waiting_time), java.util.stream.IntStream.of(0)).toArray()));
            remaining_time = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(remaining_time), java.util.stream.IntStream.of(burst_time[i])).toArray()));
            i = i + 1;
        }
        int completed = 0;
        int total_time = 0;
        while (completed != no_of_processes) {
            int[] ready_process = ((int[])(new int[]{}));
            int target_process = -1;
            int j = 0;
            while (j < no_of_processes) {
                if (arrival_time[j] <= total_time && remaining_time[j] > 0) {
                    ready_process = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ready_process), java.util.stream.IntStream.of(j)).toArray()));
                }
                j = j + 1;
            }
            if (ready_process.length > 0) {
                target_process = ready_process[0];
                int k = 0;
                while (k < ready_process.length) {
                    int idx = ready_process[k];
                    if (remaining_time[idx] < remaining_time[target_process]) {
                        target_process = idx;
                    }
                    k = k + 1;
                }
                total_time = total_time + burst_time[target_process];
                completed = completed + 1;
remaining_time[target_process] = 0;
waiting_time[target_process] = total_time - arrival_time[target_process] - burst_time[target_process];
            } else {
                total_time = total_time + 1;
            }
        }
        return waiting_time;
    }

    static int[] calculate_turnaroundtime(int[] burst_time, int no_of_processes, int[] waiting_time) {
        int[] turn_around_time = new int[0];
        int i_1 = 0;
        while (i_1 < no_of_processes) {
            turn_around_time = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(turn_around_time), java.util.stream.IntStream.of(burst_time[i_1] + waiting_time[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return turn_around_time;
    }

    static double average(int[] values) {
        int total = 0;
        int i_2 = 0;
        while (i_2 < values.length) {
            total = total + values[i_2];
            i_2 = i_2 + 1;
        }
        return (((Number)(total)).doubleValue()) / (((Number)(values.length)).doubleValue());
    }
    public static void main(String[] args) {
        System.out.println("[TEST CASE 01]");
        no_of_processes = 4;
        burst_time = ((int[])(new int[]{2, 5, 3, 7}));
        arrival_time = ((int[])(new int[]{0, 0, 0, 0}));
        waiting_time_1 = ((int[])(calculate_waitingtime(((int[])(arrival_time)), ((int[])(burst_time)), no_of_processes)));
        turn_around_time_1 = ((int[])(calculate_turnaroundtime(((int[])(burst_time)), no_of_processes, ((int[])(waiting_time_1)))));
        System.out.println("PID\tBurst Time\tArrival Time\tWaiting Time\tTurnaround Time");
        i_3 = 0;
        while (i_3 < no_of_processes) {
            int pid = i_3 + 1;
            System.out.println(_p(pid) + "\t" + _p(_geti(burst_time, i_3)) + "\t\t\t" + _p(_geti(arrival_time, i_3)) + "\t\t\t\t" + _p(_geti(waiting_time_1, i_3)) + "\t\t\t\t" + _p(_geti(turn_around_time_1, i_3)));
            i_3 = i_3 + 1;
        }
        avg_wait = average(((int[])(waiting_time_1)));
        avg_turn = average(((int[])(turn_around_time_1)));
        System.out.println("\nAverage waiting time = " + _p(avg_wait));
        System.out.println("Average turnaround time = " + _p(avg_turn));
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
