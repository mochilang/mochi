public class Main {
    static class Process {
        String process_name;
        int arrival_time;
        int stop_time;
        int burst_time;
        int waiting_time;
        int turnaround_time;
        Process(String process_name, int arrival_time, int stop_time, int burst_time, int waiting_time, int turnaround_time) {
            this.process_name = process_name;
            this.arrival_time = arrival_time;
            this.stop_time = stop_time;
            this.burst_time = burst_time;
            this.waiting_time = waiting_time;
            this.turnaround_time = turnaround_time;
        }
        Process() {}
        @Override public String toString() {
            return String.format("{'process_name': '%s', 'arrival_time': %s, 'stop_time': %s, 'burst_time': %s, 'waiting_time': %s, 'turnaround_time': %s}", String.valueOf(process_name), String.valueOf(arrival_time), String.valueOf(stop_time), String.valueOf(burst_time), String.valueOf(waiting_time), String.valueOf(turnaround_time));
        }
    }

    static class MLFQ {
        int number_of_queues;
        int[] time_slices;
        Process[] ready_queue;
        int current_time;
        Process[] finish_queue;
        MLFQ(int number_of_queues, int[] time_slices, Process[] ready_queue, int current_time, Process[] finish_queue) {
            this.number_of_queues = number_of_queues;
            this.time_slices = time_slices;
            this.ready_queue = ready_queue;
            this.current_time = current_time;
            this.finish_queue = finish_queue;
        }
        MLFQ() {}
        @Override public String toString() {
            return String.format("{'number_of_queues': %s, 'time_slices': %s, 'ready_queue': %s, 'current_time': %s, 'finish_queue': %s}", String.valueOf(number_of_queues), String.valueOf(time_slices), String.valueOf(ready_queue), String.valueOf(current_time), String.valueOf(finish_queue));
        }
    }

    static class RRResult {
        Process[] finished;
        Process[] ready;
        RRResult(Process[] finished, Process[] ready) {
            this.finished = finished;
            this.ready = ready;
        }
        RRResult() {}
        @Override public String toString() {
            return String.format("{'finished': %s, 'ready': %s}", String.valueOf(finished), String.valueOf(ready));
        }
    }

    static Process P1;
    static Process P2;
    static Process P3;
    static Process P4;
    static int number_of_queues;
    static int[] time_slices;
    static Process[] queue;
    static MLFQ mlfq;
    static Process[] finish_queue;

    static Process make_process(String name, int arrival, int burst) {
        return new Process(name, arrival, arrival, burst, 0, 0);
    }

    static MLFQ make_mlfq(int nqueues, int[] time_slices, Process[] queue, int current_time) {
        return new MLFQ(nqueues, time_slices, queue, current_time, new Process[]{});
    }

    static String[] calculate_sequence_of_finish_queue(MLFQ mlfq) {
        String[] seq = ((String[])(new String[]{}));
        int i = 0;
        while (i < mlfq.finish_queue.length) {
            Process p = mlfq.finish_queue[i];
            seq = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(seq), java.util.stream.Stream.of(p.process_name)).toArray(String[]::new)));
            i = i + 1;
        }
        return seq;
    }

    static int[] calculate_waiting_time(Process[] queue) {
        int[] times = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < queue.length) {
            Process p_1 = queue[i_1];
            times = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(times), java.util.stream.IntStream.of(p_1.waiting_time)).toArray()));
            i_1 = i_1 + 1;
        }
        return times;
    }

    static int[] calculate_turnaround_time(Process[] queue) {
        int[] times_1 = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < queue.length) {
            Process p_2 = queue[i_2];
            times_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(times_1), java.util.stream.IntStream.of(p_2.turnaround_time)).toArray()));
            i_2 = i_2 + 1;
        }
        return times_1;
    }

    static int[] calculate_completion_time(Process[] queue) {
        int[] times_2 = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < queue.length) {
            Process p_3 = queue[i_3];
            times_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(times_2), java.util.stream.IntStream.of(p_3.stop_time)).toArray()));
            i_3 = i_3 + 1;
        }
        return times_2;
    }

    static int[] calculate_remaining_burst_time_of_processes(Process[] queue) {
        int[] times_3 = ((int[])(new int[]{}));
        int i_4 = 0;
        while (i_4 < queue.length) {
            Process p_4 = queue[i_4];
            times_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(times_3), java.util.stream.IntStream.of(p_4.burst_time)).toArray()));
            i_4 = i_4 + 1;
        }
        return times_3;
    }

    static int update_waiting_time(MLFQ mlfq, Process process) {
process.waiting_time = process.waiting_time + (mlfq.current_time - process.stop_time);
        return process.waiting_time;
    }

    static Process[] first_come_first_served(MLFQ mlfq, Process[] ready_queue) {
        Process[] finished = ((Process[])(new Process[]{}));
        Process[] rq = ((Process[])(ready_queue));
        while (rq.length != 0) {
            Process cp = rq[0];
            rq = ((Process[])(java.util.Arrays.copyOfRange(rq, 1, rq.length)));
            if (mlfq.current_time < cp.arrival_time) {
mlfq.current_time = cp.arrival_time;
            }
            update_waiting_time(mlfq, cp);
mlfq.current_time = mlfq.current_time + cp.burst_time;
cp.burst_time = 0;
cp.turnaround_time = mlfq.current_time - cp.arrival_time;
cp.stop_time = mlfq.current_time;
            finished = ((Process[])(java.util.stream.Stream.concat(java.util.Arrays.stream(finished), java.util.stream.Stream.of(cp)).toArray(Process[]::new)));
        }
mlfq.finish_queue = concat(mlfq.finish_queue, finished);
        return finished;
    }

    static RRResult round_robin(MLFQ mlfq, Process[] ready_queue, int time_slice) {
        Process[] finished_1 = ((Process[])(new Process[]{}));
        Process[] rq_1 = ((Process[])(ready_queue));
        int count = rq_1.length;
        int i_5 = 0;
        while (i_5 < count) {
            Process cp_1 = rq_1[0];
            rq_1 = ((Process[])(java.util.Arrays.copyOfRange(rq_1, 1, rq_1.length)));
            if (mlfq.current_time < cp_1.arrival_time) {
mlfq.current_time = cp_1.arrival_time;
            }
            update_waiting_time(mlfq, cp_1);
            if (cp_1.burst_time > time_slice) {
mlfq.current_time = mlfq.current_time + time_slice;
cp_1.burst_time = cp_1.burst_time - time_slice;
cp_1.stop_time = mlfq.current_time;
                rq_1 = ((Process[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rq_1), java.util.stream.Stream.of(cp_1)).toArray(Process[]::new)));
            } else {
mlfq.current_time = mlfq.current_time + cp_1.burst_time;
cp_1.burst_time = 0;
cp_1.stop_time = mlfq.current_time;
cp_1.turnaround_time = mlfq.current_time - cp_1.arrival_time;
                finished_1 = ((Process[])(java.util.stream.Stream.concat(java.util.Arrays.stream(finished_1), java.util.stream.Stream.of(cp_1)).toArray(Process[]::new)));
            }
            i_5 = i_5 + 1;
        }
mlfq.finish_queue = concat(mlfq.finish_queue, finished_1);
        return new RRResult(finished_1, rq_1);
    }

    static Process[] multi_level_feedback_queue(MLFQ mlfq) {
        int i_6 = 0;
        while (i_6 < mlfq.number_of_queues - 1) {
            RRResult rr = round_robin(mlfq, ((Process[])(mlfq.ready_queue)), mlfq.time_slices[i_6]);
mlfq.ready_queue = rr.ready;
            i_6 = i_6 + 1;
        }
        first_come_first_served(mlfq, ((Process[])(mlfq.ready_queue)));
        return mlfq.finish_queue;
    }
    public static void main(String[] args) {
        P1 = make_process("P1", 0, 53);
        P2 = make_process("P2", 0, 17);
        P3 = make_process("P3", 0, 68);
        P4 = make_process("P4", 0, 24);
        number_of_queues = 3;
        time_slices = ((int[])(new int[]{17, 25}));
        queue = ((Process[])(new Process[]{P1, P2, P3, P4}));
        mlfq = make_mlfq(number_of_queues, ((int[])(time_slices)), ((Process[])(queue)), 0);
        finish_queue = ((Process[])(multi_level_feedback_queue(mlfq)));
        System.out.println("waiting time:\t\t\t" + _p(calculate_waiting_time(((Process[])(new Process[]{P1, P2, P3, P4})))));
        System.out.println("completion time:\t\t" + _p(calculate_completion_time(((Process[])(new Process[]{P1, P2, P3, P4})))));
        System.out.println("turnaround time:\t\t" + _p(calculate_turnaround_time(((Process[])(new Process[]{P1, P2, P3, P4})))));
        System.out.println("sequence of finished processes:\t" + _p(calculate_sequence_of_finish_queue(mlfq)));
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
        return out;
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
