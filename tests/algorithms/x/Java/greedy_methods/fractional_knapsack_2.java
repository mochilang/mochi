public class Main {
    static class KnapsackResult {
        double max_value;
        double[] fractions;
        KnapsackResult(double max_value, double[] fractions) {
            this.max_value = max_value;
            this.fractions = fractions;
        }
        KnapsackResult() {}
        @Override public String toString() {
            return String.format("{'max_value': %s, 'fractions': %s}", String.valueOf(max_value), String.valueOf(fractions));
        }
    }

    static double[] v;
    static double[] w;

    static long[] sort_by_ratio(long[] index, double[] ratio) {
        long i = 1L;
        while (i < index.length) {
            long key_1 = index[(int)((long)(i))];
            double key_ratio_1 = ratio[(int)((long)(key_1))];
            long j_1 = i - 1;
            while (j_1 >= 0 && ratio[(int)((long)(index[(int)((long)(j_1))]))] < key_ratio_1) {
index[(int)((long)(j_1 + 1))] = index[(int)((long)(j_1))];
                j_1 = j_1 - 1;
            }
index[(int)((long)(j_1 + 1))] = key_1;
            i = i + 1;
        }
        return index;
    }

    static KnapsackResult fractional_knapsack(double[] value, double[] weight, double capacity) {
        long n = value.length;
        long[] index_1 = ((long[])(new long[]{}));
        long i_2 = 0L;
        while (i_2 < n) {
            index_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(index_1), java.util.stream.LongStream.of(i_2)).toArray()));
            i_2 = i_2 + 1;
        }
        double[] ratio_1 = ((double[])(new double[]{}));
        i_2 = 0;
        while (i_2 < n) {
            ratio_1 = ((double[])(appendDouble(ratio_1, value[(int)((long)(i_2))] / weight[(int)((long)(i_2))])));
            i_2 = i_2 + 1;
        }
        index_1 = ((long[])(sort_by_ratio(((long[])(index_1)), ((double[])(ratio_1)))));
        double[] fractions_1 = ((double[])(new double[]{}));
        i_2 = 0;
        while (i_2 < n) {
            fractions_1 = ((double[])(appendDouble(fractions_1, 0.0)));
            i_2 = i_2 + 1;
        }
        double max_value_1 = 0.0;
        long idx_1 = 0L;
        while (idx_1 < index_1.length) {
            long item_1 = index_1[(int)((long)(idx_1))];
            if (weight[(int)((long)(item_1))] <= capacity) {
fractions_1[(int)((long)(item_1))] = 1.0;
                max_value_1 = max_value_1 + value[(int)((long)(item_1))];
                capacity = capacity - weight[(int)((long)(item_1))];
            } else {
fractions_1[(int)((long)(item_1))] = capacity / weight[(int)((long)(item_1))];
                max_value_1 = max_value_1 + value[(int)((long)(item_1))] * capacity / weight[(int)((long)(item_1))];
                break;
            }
            idx_1 = idx_1 + 1;
        }
        return new KnapsackResult(max_value_1, fractions_1);
    }
    public static void main(String[] args) {
        v = ((double[])(new double[]{1.0, 3.0, 5.0, 7.0, 9.0}));
        w = ((double[])(new double[]{0.9, 0.7, 0.5, 0.3, 0.1}));
        System.out.println(fractional_knapsack(((double[])(v)), ((double[])(w)), 5.0));
        System.out.println(fractional_knapsack(((double[])(new double[]{1.0, 3.0, 5.0, 7.0})), ((double[])(new double[]{0.9, 0.7, 0.5, 0.3})), 30.0));
        System.out.println(fractional_knapsack(((double[])(new double[]{})), ((double[])(new double[]{})), 30.0));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
