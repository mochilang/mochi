public class Main {
    static class FixedPriorityQueue {
        int[][] queues;
        FixedPriorityQueue(int[][] queues) {
            this.queues = queues;
        }
        FixedPriorityQueue() {}
        @Override public String toString() {
            return String.format("{'queues': %s}", String.valueOf(queues));
        }
    }

    static class FPQDequeueResult {
        FixedPriorityQueue queue;
        int value;
        FPQDequeueResult(FixedPriorityQueue queue, int value) {
            this.queue = queue;
            this.value = value;
        }
        FPQDequeueResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'value': %s}", String.valueOf(queue), String.valueOf(value));
        }
    }

    static class ElementPriorityQueue {
        int[] queue;
        ElementPriorityQueue(int[] queue) {
            this.queue = queue;
        }
        ElementPriorityQueue() {}
        @Override public String toString() {
            return String.format("{'queue': %s}", String.valueOf(queue));
        }
    }

    static class EPQDequeueResult {
        ElementPriorityQueue queue;
        int value;
        EPQDequeueResult(ElementPriorityQueue queue, int value) {
            this.queue = queue;
            this.value = value;
        }
        EPQDequeueResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'value': %s}", String.valueOf(queue), String.valueOf(value));
        }
    }


    static void panic(String msg) {
        System.out.println(msg);
    }

    static FixedPriorityQueue fpq_new() {
        return new FixedPriorityQueue(new int[][]{new Object[]{}, new Object[]{}, new Object[]{}});
    }

    static FixedPriorityQueue fpq_enqueue(FixedPriorityQueue fpq, int priority, int data) {
        if (priority < 0 || priority >= fpq.queues.length) {
            throw new RuntimeException(String.valueOf("Valid priorities are 0, 1, and 2"));
            return fpq;
        }
        if (fpq.queues[priority].length >= 100) {
            throw new RuntimeException(String.valueOf("Maximum queue size is 100"));
            return fpq;
        }
        int[][] qs = ((int[][])(fpq.queues));
qs[priority] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(qs[priority]), java.util.stream.IntStream.of(data)).toArray()));
fpq.queues = qs;
        return fpq;
    }

    static FPQDequeueResult fpq_dequeue(FixedPriorityQueue fpq) {
        int[][] qs_1 = ((int[][])(fpq.queues));
        int i = 0;
        while (i < qs_1.length) {
            int[] q = ((int[])(qs_1[i]));
            if (q.length > 0) {
                int val = q[0];
                int[] new_q = ((int[])(new int[]{}));
                int j = 1;
                while (j < q.length) {
                    new_q = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_q), java.util.stream.IntStream.of(q[j])).toArray()));
                    j = j + 1;
                }
qs_1[i] = ((int[])(new_q));
fpq.queues = qs_1;
                return new FPQDequeueResult(fpq, val);
            }
            i = i + 1;
        }
        throw new RuntimeException(String.valueOf("All queues are empty"));
        return new FPQDequeueResult(fpq, 0);
    }

    static String fpq_to_string(FixedPriorityQueue fpq) {
        String[] lines = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < fpq.queues.length) {
            String q_str = "[";
            int[] q_1 = ((int[])(fpq.queues[i_1]));
            int j_1 = 0;
            while (j_1 < q_1.length) {
                if (j_1 > 0) {
                    q_str = q_str + ", ";
                }
                q_str = q_str + _p(_geti(q_1, j_1));
                j_1 = j_1 + 1;
            }
            q_str = q_str + "]";
            lines = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of("Priority " + _p(i_1) + ": " + q_str)).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        String res = "";
        i_1 = 0;
        while (i_1 < lines.length) {
            if (i_1 > 0) {
                res = res + "\n";
            }
            res = res + lines[i_1];
            i_1 = i_1 + 1;
        }
        return res;
    }

    static ElementPriorityQueue epq_new() {
        return new ElementPriorityQueue(new int[]{});
    }

    static ElementPriorityQueue epq_enqueue(ElementPriorityQueue epq, int data) {
        if (epq.queue.length >= 100) {
            throw new RuntimeException(String.valueOf("Maximum queue size is 100"));
            return epq;
        }
epq.queue = java.util.stream.IntStream.concat(java.util.Arrays.stream(epq.queue), java.util.stream.IntStream.of(data)).toArray();
        return epq;
    }

    static EPQDequeueResult epq_dequeue(ElementPriorityQueue epq) {
        if (epq.queue.length == 0) {
            throw new RuntimeException(String.valueOf("The queue is empty"));
            return new EPQDequeueResult(epq, 0);
        }
        int min_val = epq.queue[0];
        int idx = 0;
        int i_2 = 1;
        while (i_2 < epq.queue.length) {
            int v = epq.queue[i_2];
            if (v < min_val) {
                min_val = v;
                idx = i_2;
            }
            i_2 = i_2 + 1;
        }
        int[] new_q_1 = ((int[])(new int[]{}));
        i_2 = 0;
        while (i_2 < epq.queue.length) {
            if (i_2 != idx) {
                new_q_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_q_1), java.util.stream.IntStream.of(epq.queue[i_2])).toArray()));
            }
            i_2 = i_2 + 1;
        }
epq.queue = new_q_1;
        return new EPQDequeueResult(epq, min_val);
    }

    static String epq_to_string(ElementPriorityQueue epq) {
        return _p(epq.queue);
    }

    static void fixed_priority_queue() {
        FixedPriorityQueue fpq = fpq_new();
        fpq = fpq_enqueue(fpq, 0, 10);
        fpq = fpq_enqueue(fpq, 1, 70);
        fpq = fpq_enqueue(fpq, 0, 100);
        fpq = fpq_enqueue(fpq, 2, 1);
        fpq = fpq_enqueue(fpq, 2, 5);
        fpq = fpq_enqueue(fpq, 1, 7);
        fpq = fpq_enqueue(fpq, 2, 4);
        fpq = fpq_enqueue(fpq, 1, 64);
        fpq = fpq_enqueue(fpq, 0, 128);
        System.out.println(fpq_to_string(fpq));
        FPQDequeueResult res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        System.out.println(fpq_to_string(fpq));
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
        res_1 = fpq_dequeue(fpq);
        fpq = res_1.queue;
        System.out.println(res_1.value);
    }

    static void element_priority_queue() {
        ElementPriorityQueue epq = epq_new();
        epq = epq_enqueue(epq, 10);
        epq = epq_enqueue(epq, 70);
        epq = epq_enqueue(epq, 100);
        epq = epq_enqueue(epq, 1);
        epq = epq_enqueue(epq, 5);
        epq = epq_enqueue(epq, 7);
        epq = epq_enqueue(epq, 4);
        epq = epq_enqueue(epq, 64);
        epq = epq_enqueue(epq, 128);
        System.out.println(epq_to_string(epq));
        EPQDequeueResult res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        System.out.println(epq_to_string(epq));
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
        res_2 = epq_dequeue(epq);
        epq = res_2.queue;
        System.out.println(res_2.value);
    }

    static void main() {
        fixed_priority_queue();
        element_priority_queue();
    }
    public static void main(String[] args) {
        main();
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
