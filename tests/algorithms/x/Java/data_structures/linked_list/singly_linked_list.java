public class Main {
    static class SinglyLinkedList {
        int[] data;
        SinglyLinkedList(int[] data) {
            this.data = data;
        }
        SinglyLinkedList() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }

    static class DeleteResult {
        SinglyLinkedList list;
        int value;
        DeleteResult(SinglyLinkedList list, int value) {
            this.list = list;
            this.value = value;
        }
        DeleteResult() {}
        @Override public String toString() {
            return String.format("{'list': %s, 'value': %s}", String.valueOf(list), String.valueOf(value));
        }
    }


    static SinglyLinkedList empty_list() {
        return new SinglyLinkedList(new int[]{});
    }

    static int length(SinglyLinkedList list) {
        return list.data.length;
    }

    static boolean is_empty(SinglyLinkedList list) {
        return list.data.length == 0;
    }

    static String to_string(SinglyLinkedList list) {
        if (list.data.length == 0) {
            return "";
        }
        String s = _p(_geti(list.data, 0));
        int i = 1;
        while (i < list.data.length) {
            s = s + " -> " + _p(_geti(list.data, i));
            i = i + 1;
        }
        return s;
    }

    static SinglyLinkedList insert_nth(SinglyLinkedList list, int index, int value) {
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
        return new SinglyLinkedList(res);
    }

    static SinglyLinkedList insert_head(SinglyLinkedList list, int value) {
        return insert_nth(list, 0, value);
    }

    static SinglyLinkedList insert_tail(SinglyLinkedList list, int value) {
        return insert_nth(list, list.data.length, value);
    }

    static DeleteResult delete_nth(SinglyLinkedList list, int index) {
        if (index < 0 || index >= list.data.length) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        int[] res_1 = ((int[])(new int[]{}));
        int val = 0;
        int i_2 = 0;
        while (i_2 < list.data.length) {
            if (i_2 == index) {
                val = list.data[i_2];
            } else {
                res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(list.data[i_2])).toArray()));
            }
            i_2 = i_2 + 1;
        }
        return new DeleteResult(new SinglyLinkedList(res_1), val);
    }

    static DeleteResult delete_head(SinglyLinkedList list) {
        return delete_nth(list, 0);
    }

    static DeleteResult delete_tail(SinglyLinkedList list) {
        return delete_nth(list, list.data.length - 1);
    }

    static int get_item(SinglyLinkedList list, int index) {
        if (index < 0 || index >= list.data.length) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        return list.data[index];
    }

    static SinglyLinkedList set_item(SinglyLinkedList list, int index, int value) {
        if (index < 0 || index >= list.data.length) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        int[] res_2 = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < list.data.length) {
            if (i_3 == index) {
                res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(value)).toArray()));
            } else {
                res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(list.data[i_3])).toArray()));
            }
            i_3 = i_3 + 1;
        }
        return new SinglyLinkedList(res_2);
    }

    static SinglyLinkedList reverse_list(SinglyLinkedList list) {
        int[] res_3 = ((int[])(new int[]{}));
        int i_4 = list.data.length - 1;
        while (i_4 >= 0) {
            res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(list.data[i_4])).toArray()));
            i_4 = i_4 - 1;
        }
        return new SinglyLinkedList(res_3);
    }

    static void main() {
        SinglyLinkedList lst = empty_list();
        int i_5 = 1;
        while (i_5 <= 5) {
            lst = insert_tail(lst, i_5);
            i_5 = i_5 + 1;
        }
        System.out.println(to_string(lst));
        lst = insert_head(lst, 0);
        System.out.println(to_string(lst));
        DeleteResult del = delete_head(lst);
        lst = del.list;
        System.out.println(_p(del.value));
        del = delete_tail(lst);
        lst = del.list;
        System.out.println(_p(del.value));
        del = delete_nth(lst, 2);
        lst = del.list;
        System.out.println(_p(del.value));
        lst = set_item(lst, 1, 99);
        System.out.println(_p(get_item(lst, 1)));
        lst = reverse_list(lst);
        System.out.println(to_string(lst));
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
