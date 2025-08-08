public class Main {
    static class CircularLinkedList {
        int[] data;
        CircularLinkedList(int[] data) {
            this.data = data;
        }
        CircularLinkedList() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }

    static class DeleteResult {
        CircularLinkedList list;
        int value;
        DeleteResult(CircularLinkedList list, int value) {
            this.list = list;
            this.value = value;
        }
        DeleteResult() {}
        @Override public String toString() {
            return String.format("{'list': %s, 'value': %s}", String.valueOf(list), String.valueOf(value));
        }
    }


    static CircularLinkedList empty_list() {
        return new CircularLinkedList(new int[]{});
    }

    static int length(CircularLinkedList list) {
        return list.data.length;
    }

    static boolean is_empty(CircularLinkedList list) {
        return list.data.length == 0;
    }

    static String to_string(CircularLinkedList list) {
        if (list.data.length == 0) {
            return "";
        }
        String s = _p(_geti(list.data, 0));
        int i = 1;
        while (i < list.data.length) {
            s = s + "->" + _p(_geti(list.data, i));
            i = i + 1;
        }
        return s;
    }

    static CircularLinkedList insert_nth(CircularLinkedList list, int index, int value) {
        if (index < 0 || index > list.data.length) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        int[] res = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < index) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(list.data[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(value)).toArray()));
        while (i_1 < list.data.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(list.data[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return new CircularLinkedList(res);
    }

    static CircularLinkedList insert_head(CircularLinkedList list, int value) {
        return insert_nth(list, 0, value);
    }

    static CircularLinkedList insert_tail(CircularLinkedList list, int value) {
        return insert_nth(list, list.data.length, value);
    }

    static DeleteResult delete_nth(CircularLinkedList list, int index) {
        if (index < 0 || index >= list.data.length) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        int[] res_1 = ((int[])(new int[]{}));
        int i_2 = 0;
        int val = 0;
        while (i_2 < list.data.length) {
            if (i_2 == index) {
                val = list.data[i_2];
            } else {
                res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(list.data[i_2])).toArray()));
            }
            i_2 = i_2 + 1;
        }
        return new DeleteResult(new CircularLinkedList(res_1), val);
    }

    static DeleteResult delete_front(CircularLinkedList list) {
        return delete_nth(list, 0);
    }

    static DeleteResult delete_tail(CircularLinkedList list) {
        return delete_nth(list, list.data.length - 1);
    }

    static void main() {
        CircularLinkedList cll = empty_list();
        int i_3 = 0;
        while (i_3 < 5) {
            cll = insert_tail(cll, i_3 + 1);
            i_3 = i_3 + 1;
        }
        System.out.println(to_string(cll));
        cll = insert_tail(cll, 6);
        System.out.println(to_string(cll));
        cll = insert_head(cll, 0);
        System.out.println(to_string(cll));
        DeleteResult res_2 = delete_front(cll);
        cll = res_2.list;
        System.out.println(res_2.value);
        res_2 = delete_tail(cll);
        cll = res_2.list;
        System.out.println(res_2.value);
        res_2 = delete_nth(cll, 2);
        cll = res_2.list;
        System.out.println(res_2.value);
        System.out.println(to_string(cll));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
