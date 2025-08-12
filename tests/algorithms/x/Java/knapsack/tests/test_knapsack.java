public class Main {

    static long knapsack(long capacity, long[] weights, long[] values, long counter) {
        if (counter == 0 || capacity == 0) {
            return 0;
        }
        if (weights[(int)((long)(counter - 1))] > capacity) {
            return knapsack(capacity, ((long[])(weights)), ((long[])(values)), counter - 1);
        }
        long left_capacity_1 = capacity - weights[(int)((long)(counter - 1))];
        long include_val_1 = values[(int)((long)(counter - 1))] + knapsack(left_capacity_1, ((long[])(weights)), ((long[])(values)), counter - 1);
        long exclude_val_1 = knapsack(capacity, ((long[])(weights)), ((long[])(values)), counter - 1);
        if (include_val_1 > exclude_val_1) {
            return include_val_1;
        }
        return exclude_val_1;
    }

    static boolean test_base_case() {
        long cap = 0L;
        long[] val_1 = ((long[])(new long[]{0}));
        long[] w_1 = ((long[])(new long[]{0}));
        long c_1 = val_1.length;
        if (knapsack(cap, ((long[])(w_1)), ((long[])(val_1)), c_1) != 0) {
            return false;
        }
        long[] val2_1 = ((long[])(new long[]{60}));
        long[] w2_1 = ((long[])(new long[]{10}));
        long c2_1 = val2_1.length;
        return knapsack(cap, ((long[])(w2_1)), ((long[])(val2_1)), c2_1) == 0;
    }

    static boolean test_easy_case() {
        long cap_1 = 3L;
        long[] val_3 = ((long[])(new long[]{1, 2, 3}));
        long[] w_3 = ((long[])(new long[]{3, 2, 1}));
        long c_3 = val_3.length;
        return knapsack(cap_1, ((long[])(w_3)), ((long[])(val_3)), c_3) == 5;
    }

    static boolean test_knapsack() {
        long cap_2 = 50L;
        long[] val_5 = ((long[])(new long[]{60, 100, 120}));
        long[] w_5 = ((long[])(new long[]{10, 20, 30}));
        long c_5 = val_5.length;
        return knapsack(cap_2, ((long[])(w_5)), ((long[])(val_5)), c_5) == 220;
    }
    public static void main(String[] args) {
        System.out.println(test_base_case());
        System.out.println(test_easy_case());
        System.out.println(test_knapsack());
        System.out.println(true ? "True" : "False");
    }
}
