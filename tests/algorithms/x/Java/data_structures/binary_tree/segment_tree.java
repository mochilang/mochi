public class Main {
    static int[] A = new int[0];
    static int N = 0;
    static int[] st = new int[0];
    static int NEG_INF;

    static int left_child(int idx) {
        return idx * 2;
    }

    static int right_child(int idx) {
        return idx * 2 + 1;
    }

    static void build(int idx, int left, int right) {
        if (left == right) {
st[idx] = A[left];
        } else {
            int mid = (left + right) / 2;
            build(left_child(idx), left, mid);
            build(right_child(idx), mid + 1, right);
            int left_val = st[left_child(idx)];
            int right_val = st[right_child(idx)];
st[idx] = left_val > right_val ? left_val : right_val;
        }
    }

    static boolean update_recursive(int idx, int left, int right, int a, int b, int val) {
        if (right < a || left > b) {
            return true;
        }
        if (left == right) {
st[idx] = val;
            return true;
        }
        int mid_1 = (left + right) / 2;
        update_recursive(left_child(idx), left, mid_1, a, b, val);
        update_recursive(right_child(idx), mid_1 + 1, right, a, b, val);
        int left_val_1 = st[left_child(idx)];
        int right_val_1 = st[right_child(idx)];
st[idx] = left_val_1 > right_val_1 ? left_val_1 : right_val_1;
        return true;
    }

    static boolean update(int a, int b, int val) {
        return update_recursive(1, 0, N - 1, a - 1, b - 1, val);
    }

    static int query_recursive(int idx, int left, int right, int a, int b) {
        if (right < a || left > b) {
            return NEG_INF;
        }
        if (left >= a && right <= b) {
            return st[idx];
        }
        int mid_2 = (left + right) / 2;
        int q1 = query_recursive(left_child(idx), left, mid_2, a, b);
        int q2 = query_recursive(right_child(idx), mid_2 + 1, right, a, b);
        return q1 > q2 ? q1 : q2;
    }

    static int query(int a, int b) {
        return query_recursive(1, 0, N - 1, a - 1, b - 1);
    }

    static void show_data() {
        int i = 0;
        int[] show_list = ((int[])(new int[]{}));
        while (i < N) {
            show_list = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(show_list), java.util.stream.IntStream.of(query(i + 1, i + 1))).toArray()));
            i = i + 1;
        }
        System.out.println(java.util.Arrays.toString(show_list));
    }

    static void main() {
        A = ((int[])(new int[]{1, 2, -4, 7, 3, -5, 6, 11, -20, 9, 14, 15, 5, 2, -8}));
        N = A.length;
        int i_1 = 0;
        while (i_1 < 4 * N) {
            st = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(st), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        if (N > 0) {
            build(1, 0, N - 1);
        }
        System.out.println(query(4, 6));
        System.out.println(query(7, 11));
        System.out.println(query(7, 12));
        update(1, 3, 111);
        System.out.println(query(1, 15));
        update(7, 8, 235);
        show_data();
    }
    public static void main(String[] args) {
        A = ((int[])(new int[]{}));
        N = 0;
        st = ((int[])(new int[]{}));
        NEG_INF = -1000000000;
        main();
    }
}
