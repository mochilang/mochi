public class Main {
    static class Stack {
        int[] items;
        int limit;
        Stack(int[] items, int limit) {
            this.items = items;
            this.limit = limit;
        }
        Stack() {}
        @Override public String toString() {
            return String.format("{'items': %s, 'limit': %s}", String.valueOf(items), String.valueOf(limit));
        }
    }


    static Stack make_stack(int limit) {
        return new Stack(new int[]{}, limit);
    }

    static boolean is_empty(Stack s) {
        return s.items.length == 0;
    }

    static int size(Stack s) {
        return s.items.length;
    }

    static boolean is_full(Stack s) {
        return s.items.length >= s.limit;
    }

    static void push(Stack s, int item) {
        if (((Boolean)(is_full(s)))) {
            throw new RuntimeException(String.valueOf("stack overflow"));
        }
s.items = java.util.stream.IntStream.concat(java.util.Arrays.stream(s.items), java.util.stream.IntStream.of(item)).toArray();
    }

    static int pop(Stack s) {
        if (((Boolean)(is_empty(s)))) {
            throw new RuntimeException(String.valueOf("stack underflow"));
        }
        int n = s.items.length;
        int val = s.items[n - 1];
s.items = java.util.Arrays.copyOfRange(s.items, 0, n - 1);
        return val;
    }

    static int peek(Stack s) {
        if (((Boolean)(is_empty(s)))) {
            throw new RuntimeException(String.valueOf("peek from empty stack"));
        }
        return s.items[s.items.length - 1];
    }

    static boolean contains(Stack s, int item) {
        int i = 0;
        while (i < s.items.length) {
            if (s.items[i] == item) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String stack_repr(Stack s) {
        return _p(s.items);
    }

    static void main() {
        Stack s = make_stack(5);
        System.out.println(_p(is_empty(s)));
        push(s, 0);
        push(s, 1);
        push(s, 2);
        System.out.println(_p(peek(s)));
        System.out.println(_p(size(s)));
        System.out.println(_p(is_full(s)));
        push(s, 3);
        push(s, 4);
        System.out.println(_p(is_full(s)));
        System.out.println(stack_repr(s));
        System.out.println(_p(pop(s)));
        System.out.println(_p(peek(s)));
        System.out.println(_p(contains(s, 1)));
        System.out.println(_p(contains(s, 9)));
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
}
