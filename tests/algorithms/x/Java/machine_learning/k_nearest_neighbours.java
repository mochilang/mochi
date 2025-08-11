public class Main {
    static class PointLabel {
        double[] point;
        long label;
        PointLabel(double[] point, long label) {
            this.point = point;
            this.label = label;
        }
        PointLabel() {}
        @Override public String toString() {
            return String.format("{'point': %s, 'label': %s}", String.valueOf(point), String.valueOf(label));
        }
    }

    static class KNN {
        PointLabel[] data;
        String[] labels;
        KNN(PointLabel[] data, String[] labels) {
            this.data = data;
            this.labels = labels;
        }
        KNN() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'labels': %s}", String.valueOf(data), String.valueOf(labels));
        }
    }

    static class DistLabel {
        double dist;
        long label;
        DistLabel(double dist, long label) {
            this.dist = dist;
            this.label = label;
        }
        DistLabel() {}
        @Override public String toString() {
            return String.format("{'dist': %s, 'label': %s}", String.valueOf(dist), String.valueOf(label));
        }
    }

    static double[][] train_X;
    static long[] train_y;
    static String[] classes;
    static KNN knn;
    static double[] point;

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0;
        while (i_1 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static KNN make_knn(double[][] train_data, long[] train_target, String[] class_labels) {
        PointLabel[] items = ((PointLabel[])(new PointLabel[]{}));
        long i_3 = 0;
        while (i_3 < train_data.length) {
            PointLabel pl_1 = new PointLabel(train_data[(int)(i_3)], train_target[(int)(i_3)]);
            items = ((PointLabel[])(java.util.stream.Stream.concat(java.util.Arrays.stream(items), java.util.stream.Stream.of(pl_1)).toArray(PointLabel[]::new)));
            i_3 = i_3 + 1;
        }
        return new KNN(items, class_labels);
    }

    static double euclidean_distance(double[] a, double[] b) {
        double sum = 0.0;
        long i_5 = 0;
        while (i_5 < a.length) {
            double diff_1 = a[(int)(i_5)] - b[(int)(i_5)];
            sum = sum + diff_1 * diff_1;
            i_5 = i_5 + 1;
        }
        return sqrtApprox(sum);
    }

    static String classify(KNN knn, double[] pred_point, long k) {
        DistLabel[] distances = ((DistLabel[])(new DistLabel[]{}));
        long i_7 = 0;
        while (i_7 < knn.data.length) {
            double d_1 = euclidean_distance(((double[])(knn.data[(int)(i_7)].point)), ((double[])(pred_point)));
            distances = ((DistLabel[])(java.util.stream.Stream.concat(java.util.Arrays.stream(distances), java.util.stream.Stream.of(new DistLabel(d_1, knn.data[(int)(i_7)].label))).toArray(DistLabel[]::new)));
            i_7 = i_7 + 1;
        }
        long[] votes_1 = ((long[])(new long[]{}));
        long count_1 = 0;
        while (count_1 < k) {
            long min_index_1 = 0;
            long j_1 = 1;
            while (j_1 < distances.length) {
                if (distances[(int)(j_1)].dist < distances[(int)(min_index_1)].dist) {
                    min_index_1 = j_1;
                }
                j_1 = j_1 + 1;
            }
            votes_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(votes_1), java.util.stream.LongStream.of(distances[(int)(min_index_1)].label)).toArray()));
distances[(int)(min_index_1)].dist = 1000000000000000000.0;
            count_1 = count_1 + 1;
        }
        long[] tally_1 = ((long[])(new long[]{}));
        long t_1 = 0;
        while (t_1 < knn.labels.length) {
            tally_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(tally_1), java.util.stream.LongStream.of(0)).toArray()));
            t_1 = t_1 + 1;
        }
        long v_1 = 0;
        while (v_1 < votes_1.length) {
            long lbl_1 = votes_1[(int)(v_1)];
tally_1[(int)(lbl_1)] = tally_1[(int)(lbl_1)] + 1;
            v_1 = v_1 + 1;
        }
        long max_idx_1 = 0;
        long m_1 = 1;
        while (m_1 < tally_1.length) {
            if (tally_1[(int)(m_1)] > tally_1[(int)(max_idx_1)]) {
                max_idx_1 = m_1;
            }
            m_1 = m_1 + 1;
        }
        return knn.labels[(int)(max_idx_1)];
    }
    public static void main(String[] args) {
        train_X = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{1.0, 0.0}, new double[]{0.0, 1.0}, new double[]{0.5, 0.5}, new double[]{3.0, 3.0}, new double[]{2.0, 3.0}, new double[]{3.0, 2.0}}));
        train_y = ((long[])(new long[]{0, 0, 0, 0, 1, 1, 1}));
        classes = ((String[])(new String[]{"A", "B"}));
        knn = make_knn(((double[][])(train_X)), ((long[])(train_y)), ((String[])(classes)));
        point = ((double[])(new double[]{1.2, 1.2}));
        System.out.println(classify(knn, ((double[])(point)), 5));
    }
}
