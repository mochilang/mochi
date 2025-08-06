public class Main {
    static class LinkedList {
        int[] next;
        int head;
        LinkedList(int[] next, int head) {
            this.next = next;
            this.head = head;
        }
        LinkedList() {}
        @Override public String toString() {
            return String.format("{'next': %s, 'head': %s}", String.valueOf(next), String.valueOf(head));
        }
    }

    static int NULL;

    static LinkedList empty_list() {
        return new LinkedList(new int[]{}, NULL);
    }

    static LinkedList add_node(LinkedList list, int value) {
        int[] nexts = ((int[])(list.next));
        int new_index = nexts.length;
        nexts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nexts), java.util.stream.IntStream.of(NULL)).toArray()));
        if (list.head == NULL) {
            return new LinkedList(nexts, new_index);
        }
        int last = list.head;
        while (nexts[last] != NULL) {
            last = nexts[last];
        }
        int[] new_nexts = ((int[])(new int[]{}));
        int i = 0;
        while (i < nexts.length) {
            if (i == last) {
                new_nexts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_nexts), java.util.stream.IntStream.of(new_index)).toArray()));
            } else {
                new_nexts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_nexts), java.util.stream.IntStream.of(nexts[i])).toArray()));
            }
            i = i + 1;
        }
        return new LinkedList(new_nexts, list.head);
    }

    static LinkedList set_next(LinkedList list, int index, int next_index) {
        int[] nexts_1 = ((int[])(list.next));
        int[] new_nexts_1 = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < nexts_1.length) {
            if (i_1 == index) {
                new_nexts_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_nexts_1), java.util.stream.IntStream.of(next_index)).toArray()));
            } else {
                new_nexts_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_nexts_1), java.util.stream.IntStream.of(nexts_1[i_1])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return new LinkedList(new_nexts_1, list.head);
    }

    static boolean detect_cycle(LinkedList list) {
        if (list.head == NULL) {
            return false;
        }
        int[] nexts_2 = ((int[])(list.next));
        int slow = list.head;
        int fast = list.head;
        while (fast != NULL && nexts_2[fast] != NULL) {
            slow = nexts_2[slow];
            fast = nexts_2[nexts_2[fast]];
            if (slow == fast) {
                return true;
            }
        }
        return false;
    }

    static void main() {
        LinkedList ll = empty_list();
        ll = add_node(ll, 1);
        ll = add_node(ll, 2);
        ll = add_node(ll, 3);
        ll = add_node(ll, 4);
        ll = set_next(ll, 3, 1);
        System.out.println(detect_cycle(ll));
    }
    public static void main(String[] args) {
        NULL = 0 - 1;
        main();
    }
}
