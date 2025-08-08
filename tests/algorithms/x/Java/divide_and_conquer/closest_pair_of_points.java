public class Main {
    static double[][] points_1;

    static double abs(double x) {
        if (x < 0.0) {
            return 0.0 - x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double euclidean_distance_sqr(double[] p1, double[] p2) {
        double dx = p1[0] - p2[0];
        double dy = p1[1] - p2[1];
        return dx * dx + dy * dy;
    }

    static double[][] column_based_sort(double[][] arr, int column) {
        double[][] points = ((double[][])(arr));
        int i_1 = 0;
        while (i_1 < points.length) {
            int j = 0;
            while (j < points.length - 1) {
                if (points[j][column] > points[j + 1][column]) {
                    double[] tmp = ((double[])(points[j]));
points[j] = ((double[])(points[j + 1]));
points[j + 1] = ((double[])(tmp));
                }
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        return points;
    }

    static double dis_between_closest_pair(double[][] points, int count, double min_dis) {
        int i_2 = 0;
        while (i_2 < count - 1) {
            int j_1 = i_2 + 1;
            while (j_1 < count) {
                double current = euclidean_distance_sqr(((double[])(points[i_2])), ((double[])(points[j_1])));
                if (current < min_dis) {
                    min_dis = current;
                }
                j_1 = j_1 + 1;
            }
            i_2 = i_2 + 1;
        }
        return min_dis;
    }

    static double dis_between_closest_in_strip(double[][] points, int count, double min_dis) {
        int i_start = 0;
        if (6 < count - 1) {
            i_start = 6;
        } else {
            i_start = count - 1;
        }
        int i_3 = i_start;
        while (i_3 < count) {
            int j_start = 0;
            if (i_3 - 6 > 0) {
                j_start = i_3 - 6;
            }
            int j_2 = j_start;
            while (j_2 < i_3) {
                double current_1 = euclidean_distance_sqr(((double[])(points[i_3])), ((double[])(points[j_2])));
                if (current_1 < min_dis) {
                    min_dis = current_1;
                }
                j_2 = j_2 + 1;
            }
            i_3 = i_3 + 1;
        }
        return min_dis;
    }

    static double closest_pair_of_points_sqr(double[][] px, double[][] py, int count) {
        if (count <= 3) {
            return dis_between_closest_pair(((double[][])(px)), count, 1000000000000000000.0);
        }
        int mid = Math.floorDiv(count, 2);
        double left = closest_pair_of_points_sqr(((double[][])(px)), ((double[][])(java.util.Arrays.copyOfRange(py, 0, mid))), mid);
        double right = closest_pair_of_points_sqr(((double[][])(py)), ((double[][])(java.util.Arrays.copyOfRange(py, mid, count))), count - mid);
        double best = left;
        if (right < best) {
            best = right;
        }
        double[][] strip = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < px.length) {
            if (Math.abs(px[i_4][0] - px[mid][0]) < best) {
                strip = ((double[][])(appendObj((double[][])strip, px[i_4])));
            }
            i_4 = i_4 + 1;
        }
        double strip_best = dis_between_closest_in_strip(((double[][])(strip)), strip.length, best);
        if (strip_best < best) {
            best = strip_best;
        }
        return best;
    }

    static double closest_pair_of_points(double[][] points, int count) {
        double[][] points_sorted_on_x = ((double[][])(column_based_sort(((double[][])(points)), 0)));
        double[][] points_sorted_on_y = ((double[][])(column_based_sort(((double[][])(points)), 1)));
        double dist_sqr = closest_pair_of_points_sqr(((double[][])(points_sorted_on_x)), ((double[][])(points_sorted_on_y)), count);
        return sqrtApprox(dist_sqr);
    }
    public static void main(String[] args) {
        points_1 = ((double[][])(new double[][]{new double[]{2.0, 3.0}, new double[]{12.0, 30.0}, new double[]{40.0, 50.0}, new double[]{5.0, 1.0}, new double[]{12.0, 10.0}, new double[]{3.0, 4.0}}));
        System.out.println("Distance: " + _p(closest_pair_of_points(((double[][])(points_1)), points_1.length)));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
