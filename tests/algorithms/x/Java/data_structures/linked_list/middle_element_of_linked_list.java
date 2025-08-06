public class Main {
    static class List {
        int[] data;
        List(int[] data) {
            this.data = data;
        }
        List() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }


    static List empty_list() {
        return new List(new int[]{});
    }

    static List push(List lst, int value) {
        int[] res = ((int[])(new int[]{value}));
        int i = 0;
        while (i < lst.data.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(lst.data[i])).toArray()));
            i = i + 1;
        }
        return new List(res);
    }

    static int middle_element(List lst) {
        int n = lst.data.length;
        if (n == 0) {
            System.out.println("No element found.");
            return 0;
        }
        int slow = 0;
        int fast = 0;
        while (fast + 1 < n) {
            fast = fast + 2;
            slow = slow + 1;
        }
        return lst.data[slow];
    }

    static void main() {
        List lst = empty_list();
        middle_element(lst);
        lst = push(lst, 5);
        System.out.println(5);
        lst = push(lst, 6);
        System.out.println(6);
        lst = push(lst, 8);
        System.out.println(8);
        lst = push(lst, 8);
        System.out.println(8);
        lst = push(lst, 10);
        System.out.println(10);
        lst = push(lst, 12);
        System.out.println(12);
        lst = push(lst, 17);
        System.out.println(17);
        lst = push(lst, 7);
        System.out.println(7);
        lst = push(lst, 3);
        System.out.println(3);
        lst = push(lst, 20);
        System.out.println(20);
        lst = push(lst, -20);
        System.out.println(-20);
        System.out.println(middle_element(lst));
    }
    public static void main(String[] args) {
        main();
    }
}
