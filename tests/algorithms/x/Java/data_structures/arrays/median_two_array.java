public class Main {

    static double[] sortFloats(double[] xs) {
        double[] arr = ((double[])(xs));
        int i = 0;
        while (i < arr.length) {
            int j = 0;
            while (j < arr.length - 1) {
                if (arr[j] > arr[j + 1]) {
                    double t = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = t;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static double find_median_sorted_arrays(double[] nums1, double[] nums2) {
        if (nums1.length == 0 && nums2.length == 0) {
            throw new RuntimeException(String.valueOf("Both input arrays are empty."));
        }
        double[] merged = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < nums1.length) {
            merged = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(merged), java.util.stream.DoubleStream.of(nums1[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        int j_1 = 0;
        while (j_1 < nums2.length) {
            merged = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(merged), java.util.stream.DoubleStream.of(nums2[j_1])).toArray()));
            j_1 = j_1 + 1;
        }
        double[] sorted = ((double[])(sortFloats(((double[])(merged)))));
        int total = sorted.length;
        if (Math.floorMod(total, 2) == 1) {
            return sorted[total / 2];
        }
        double middle1 = sorted[total / 2 - 1];
        double middle2 = sorted[total / 2];
        return (middle1 + middle2) / 2.0;
    }
    public static void main(String[] args) {
        System.out.println(find_median_sorted_arrays(((double[])(new double[]{1.0, 3.0})), ((double[])(new double[]{2.0}))));
        System.out.println(find_median_sorted_arrays(((double[])(new double[]{1.0, 2.0})), ((double[])(new double[]{3.0, 4.0}))));
        System.out.println(find_median_sorted_arrays(((double[])(new double[]{0.0, 0.0})), ((double[])(new double[]{0.0, 0.0}))));
        System.out.println(find_median_sorted_arrays(((double[])(new double[]{})), ((double[])(new double[]{1.0}))));
        System.out.println(find_median_sorted_arrays(((double[])(new double[]{-1000.0})), ((double[])(new double[]{1000.0}))));
        System.out.println(find_median_sorted_arrays(((double[])(new double[]{-1.1, -2.2})), ((double[])(new double[]{-3.3, -4.4}))));
    }
}
