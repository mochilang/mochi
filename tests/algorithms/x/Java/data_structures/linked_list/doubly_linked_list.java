public class Main {
    static class DoublyLinkedList {
        int[] data;
        DoublyLinkedList(int[] data) {
            this.data = data;
        }
        DoublyLinkedList() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }

    static class DeleteResult {
        DoublyLinkedList list;
        int value;
        DeleteResult(DoublyLinkedList list, int value) {
            this.list = list;
            this.value = value;
        }
        DeleteResult() {}
        @Override public String toString() {
            return String.format("{'list': %s, 'value': %s}", String.valueOf(list), String.valueOf(value));
        }
    }


    static DoublyLinkedList empty_list() {
        return new DoublyLinkedList(new int[]{});
    }

    static int length(DoublyLinkedList list) {
        return list.data.length;
    }

    static boolean is_empty(DoublyLinkedList list) {
        return list.data.length == 0;
    }

    static String to_string(DoublyLinkedList list) {
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

    static DoublyLinkedList insert_nth(DoublyLinkedList list, int index, int value) {
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
        return new DoublyLinkedList(res);
    }

    static DoublyLinkedList insert_head(DoublyLinkedList list, int value) {
        return insert_nth(list, 0, value);
    }

    static DoublyLinkedList insert_tail(DoublyLinkedList list, int value) {
        return insert_nth(list, list.data.length, value);
    }

    static DeleteResult delete_nth(DoublyLinkedList list, int index) {
        if (index < 0 || index >= list.data.length) {
            throw new RuntimeException(String.valueOf("index out of range"));
        }
        int[] res_1 = ((int[])(new int[]{}));
        int i_2 = 0;
        int removed = 0;
        while (i_2 < list.data.length) {
            if (i_2 == index) {
                removed = list.data[i_2];
            } else {
                res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(list.data[i_2])).toArray()));
            }
            i_2 = i_2 + 1;
        }
        return new DeleteResult(new DoublyLinkedList(res_1), removed);
    }

    static DeleteResult delete_head(DoublyLinkedList list) {
        return delete_nth(list, 0);
    }

    static DeleteResult delete_tail(DoublyLinkedList list) {
        return delete_nth(list, list.data.length - 1);
    }

    static DeleteResult delete_value(DoublyLinkedList list, int value) {
        int idx = 0;
        boolean found = false;
        while (idx < list.data.length) {
            if (list.data[idx] == value) {
                found = true;
                break;
            }
            idx = idx + 1;
        }
        if (!found) {
            throw new RuntimeException(String.valueOf("value not found"));
        }
        return delete_nth(list, idx);
    }

    static void main() {
        DoublyLinkedList dll = empty_list();
        dll = insert_tail(dll, 1);
        dll = insert_tail(dll, 2);
        dll = insert_tail(dll, 3);
        System.out.println(to_string(dll));
        dll = insert_head(dll, 0);
        System.out.println(to_string(dll));
        dll = insert_nth(dll, 2, 9);
        System.out.println(to_string(dll));
        DeleteResult res_2 = delete_nth(dll, 2);
        dll = res_2.list;
        System.out.println(res_2.value);
        System.out.println(to_string(dll));
        res_2 = delete_tail(dll);
        dll = res_2.list;
        System.out.println(res_2.value);
        System.out.println(to_string(dll));
        res_2 = delete_value(dll, 1);
        dll = res_2.list;
        System.out.println(res_2.value);
        System.out.println(to_string(dll));
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
