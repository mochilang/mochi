public class Main {
    static class LinkedList {
        int[] data;
        LinkedList(int[] data) {
            this.data = data;
        }
        LinkedList() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }


    static LinkedList empty_list() {
        return new LinkedList(new int[]{});
    }

    static LinkedList push(LinkedList list, int value) {
        int[] res = ((int[])(new int[]{value}));
        res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(list.data)).toArray()));
        return new LinkedList(res);
    }

    static LinkedList swap_nodes(LinkedList list, int v1, int v2) {
        if (v1 == v2) {
            return list;
        }
        int idx1 = 0 - 1;
        int idx2 = 0 - 1;
        int i = 0;
        while (i < list.data.length) {
            if (list.data[i] == v1 && idx1 == 0 - 1) {
                idx1 = i;
            }
            if (list.data[i] == v2 && idx2 == 0 - 1) {
                idx2 = i;
            }
            i = i + 1;
        }
        if (idx1 == 0 - 1 || idx2 == 0 - 1) {
            return list;
        }
        int[] res_1 = ((int[])(list.data));
        int temp = res_1[idx1];
res_1[idx1] = res_1[idx2];
res_1[idx2] = temp;
        return new LinkedList(res_1);
    }

    static String to_string(LinkedList list) {
        return _p(list.data);
    }

    static void main() {
        LinkedList ll = empty_list();
        int i_1 = 5;
        while (i_1 > 0) {
            ll = push(ll, i_1);
            i_1 = i_1 - 1;
        }
        System.out.println("Original Linked List: " + String.valueOf(to_string(ll)));
        ll = swap_nodes(ll, 1, 4);
        System.out.println("Modified Linked List: " + String.valueOf(to_string(ll)));
        System.out.println("After swapping the nodes whose data is 1 and 4.");
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
