public class Main {
    static class Job {
        int id;
        int deadline;
        int profit;
        Job(int id, int deadline, int profit) {
            this.id = id;
            this.deadline = deadline;
            this.profit = profit;
        }
        Job() {}
        @Override public String toString() {
            return String.format("{'id': %s, 'deadline': %s, 'profit': %s}", String.valueOf(id), String.valueOf(deadline), String.valueOf(profit));
        }
    }

    static Job[] jobs1 = new Job[0];
    static Job[] jobs2 = new Job[0];

    static Job[] sort_jobs_by_profit(Job[] jobs) {
        Job[] js = ((Job[])(jobs));
        int i = 0;
        while (i < js.length) {
            int j = 0;
            while (j < js.length - i - 1) {
                Job a = js[j];
                Job b = js[j + 1];
                if (a.profit < b.profit) {
js[j] = b;
js[j + 1] = a;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return js;
    }

    static int max_deadline(Job[] jobs) {
        int max_d = 0;
        int i_1 = 0;
        while (i_1 < jobs.length) {
            Job job = jobs[i_1];
            int d = job.deadline;
            if (d > max_d) {
                max_d = d;
            }
            i_1 = i_1 + 1;
        }
        return max_d;
    }

    static int[] job_sequencing_with_deadlines(Job[] jobs) {
        Job[] js_1 = ((Job[])(sort_jobs_by_profit(((Job[])(jobs)))));
        int max_d_1 = max_deadline(((Job[])(js_1)));
        int[] time_slots = ((int[])(new int[]{}));
        int t = 0;
        while (t < max_d_1) {
            time_slots = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(time_slots), java.util.stream.IntStream.of(0 - 1)).toArray()));
            t = t + 1;
        }
        int count = 0;
        int max_profit = 0;
        int i_2 = 0;
        while (i_2 < js_1.length) {
            Job job_1 = js_1[i_2];
            int j_1 = job_1.deadline - 1;
            while (j_1 >= 0) {
                if (time_slots[j_1] == 0 - 1) {
time_slots[j_1] = job_1.id;
                    count = count + 1;
                    max_profit = max_profit + job_1.profit;
                    break;
                }
                j_1 = j_1 - 1;
            }
            i_2 = i_2 + 1;
        }
        int[] result = ((int[])(new int[]{}));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(count)).toArray()));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(max_profit)).toArray()));
        return result;
    }
    public static void main(String[] args) {
        jobs1 = ((Job[])(new Job[]{}));
        jobs1 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs1), java.util.stream.Stream.of(new Job(1, 4, 20))).toArray(Job[]::new)));
        jobs1 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs1), java.util.stream.Stream.of(new Job(2, 1, 10))).toArray(Job[]::new)));
        jobs1 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs1), java.util.stream.Stream.of(new Job(3, 1, 40))).toArray(Job[]::new)));
        jobs1 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs1), java.util.stream.Stream.of(new Job(4, 1, 30))).toArray(Job[]::new)));
        System.out.println(_p(job_sequencing_with_deadlines(((Job[])(jobs1)))));
        jobs2 = ((Job[])(new Job[]{}));
        jobs2 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs2), java.util.stream.Stream.of(new Job(1, 2, 100))).toArray(Job[]::new)));
        jobs2 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs2), java.util.stream.Stream.of(new Job(2, 1, 19))).toArray(Job[]::new)));
        jobs2 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs2), java.util.stream.Stream.of(new Job(3, 2, 27))).toArray(Job[]::new)));
        jobs2 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs2), java.util.stream.Stream.of(new Job(4, 1, 25))).toArray(Job[]::new)));
        jobs2 = ((Job[])(java.util.stream.Stream.concat(java.util.Arrays.stream(jobs2), java.util.stream.Stream.of(new Job(5, 1, 15))).toArray(Job[]::new)));
        System.out.println(_p(job_sequencing_with_deadlines(((Job[])(jobs2)))));
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
