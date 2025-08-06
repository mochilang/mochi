public class Main {
    static class StackWithQueues {
        int[] main_queue;
        int[] temp_queue;
        StackWithQueues(int[] main_queue, int[] temp_queue) {
            this.main_queue = main_queue;
            this.temp_queue = temp_queue;
        }
        StackWithQueues() {}
        @Override public String toString() {
            return String.format("{'main_queue': %s, 'temp_queue': %s}", String.valueOf(main_queue), String.valueOf(temp_queue));
        }
    }

    static StackWithQueues stack = null;

    static StackWithQueues make_stack() {
        return new StackWithQueues(new int[]{}, new int[]{});
    }

    static void push(StackWithQueues s, int item) {
s.temp_queue = java.util.stream.IntStream.concat(java.util.Arrays.stream(s.temp_queue), java.util.stream.IntStream.of(item)).toArray();
        while (s.main_queue.length > 0) {
s.temp_queue = java.util.stream.IntStream.concat(java.util.Arrays.stream(s.temp_queue), java.util.stream.IntStream.of(s.main_queue[0])).toArray();
s.main_queue = java.util.Arrays.copyOfRange(s.main_queue, 1, s.main_queue.length);
        }
        int[] new_main = ((int[])(s.temp_queue));
s.temp_queue = s.main_queue;
s.main_queue = new_main;
    }

    static int pop(StackWithQueues s) {
        if (s.main_queue.length == 0) {
            throw new RuntimeException(String.valueOf("pop from empty stack"));
        }
        int item = s.main_queue[0];
s.main_queue = java.util.Arrays.copyOfRange(s.main_queue, 1, s.main_queue.length);
        return item;
    }

    static int peek(StackWithQueues s) {
        if (s.main_queue.length == 0) {
            throw new RuntimeException(String.valueOf("peek from empty stack"));
        }
        return s.main_queue[0];
    }

    static boolean is_empty(StackWithQueues s) {
        return s.main_queue.length == 0;
    }
    public static void main(String[] args) {
        stack = make_stack();
        push(stack, 1);
        push(stack, 2);
        push(stack, 3);
        System.out.println(_p(peek(stack)));
        System.out.println(_p(pop(stack)));
        System.out.println(_p(peek(stack)));
        System.out.println(_p(pop(stack)));
        System.out.println(_p(pop(stack)));
        System.out.println(_p(is_empty(stack)));
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
