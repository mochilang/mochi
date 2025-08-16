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

    static double[][] train_X = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{1.0, 0.0}, new double[]{0.0, 1.0}, new double[]{0.5, 0.5}, new double[]{3.0, 3.0}, new double[]{2.0, 3.0}, new double[]{3.0, 2.0}}));
    static long[] train_y = ((long[])(new long[]{0, 0, 0, 0, 1, 1, 1}));
    static String[] classes = ((String[])(new String[]{"A", "B"}));
    static KNN knn;
    static double[] point = ((double[])(new double[]{1.2, 1.2}));

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static KNN make_knn(double[][] train_data, long[] train_target, String[] class_labels) {
        PointLabel[] items = ((PointLabel[])(new PointLabel[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(train_data.length)) {
            PointLabel pl_1 = new PointLabel(train_data[(int)((long)(i_3))], train_target[(int)((long)(i_3))]);
            items = ((PointLabel[])(java.util.stream.Stream.concat(java.util.Arrays.stream(items), java.util.stream.Stream.of(pl_1)).toArray(PointLabel[]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return new KNN(items, class_labels);
    }

    static double euclidean_distance(double[] a, double[] b) {
        double sum = (double)(0.0);
        long i_5 = 0L;
        while ((long)(i_5) < (long)(a.length)) {
            double diff_1 = (double)((double)(a[(int)((long)(i_5))]) - (double)(b[(int)((long)(i_5))]));
            sum = (double)((double)(sum) + (double)((double)(diff_1) * (double)(diff_1)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return sqrtApprox((double)(sum));
    }

    static String classify(KNN knn, double[] pred_point, long k) {
        DistLabel[] distances = ((DistLabel[])(new DistLabel[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(knn.data.length)) {
            double d_1 = (double)(euclidean_distance(((double[])(knn.data[(int)((long)(i_7))].point)), ((double[])(pred_point))));
            distances = ((DistLabel[])(java.util.stream.Stream.concat(java.util.Arrays.stream(distances), java.util.stream.Stream.of(new DistLabel(d_1, knn.data[(int)((long)(i_7))].label))).toArray(DistLabel[]::new)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        long[] votes_1 = ((long[])(new long[]{}));
        long count_1 = 0L;
        while ((long)(count_1) < (long)(k)) {
            long min_index_1 = 0L;
            long j_1 = 1L;
            while ((long)(j_1) < (long)(distances.length)) {
                if ((double)(distances[(int)((long)(j_1))].dist) < (double)(distances[(int)((long)(min_index_1))].dist)) {
                    min_index_1 = (long)(j_1);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            votes_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(votes_1), java.util.stream.LongStream.of((long)(distances[(int)((long)(min_index_1))].label))).toArray()));
distances[(int)((long)(min_index_1))].dist = 1000000000000000000.0;
            count_1 = (long)((long)(count_1) + 1L);
        }
        long[] tally_1 = ((long[])(new long[]{}));
        long t_1 = 0L;
        while ((long)(t_1) < (long)(knn.labels.length)) {
            tally_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(tally_1), java.util.stream.LongStream.of(0L)).toArray()));
            t_1 = (long)((long)(t_1) + 1L);
        }
        long v_1 = 0L;
        while ((long)(v_1) < (long)(votes_1.length)) {
            long lbl_1 = (long)(votes_1[(int)((long)(v_1))]);
tally_1[(int)((long)(lbl_1))] = (long)((long)(tally_1[(int)((long)(lbl_1))]) + 1L);
            v_1 = (long)((long)(v_1) + 1L);
        }
        long max_idx_1 = 0L;
        long m_1 = 1L;
        while ((long)(m_1) < (long)(tally_1.length)) {
            if ((long)(tally_1[(int)((long)(m_1))]) > (long)(tally_1[(int)((long)(max_idx_1))])) {
                max_idx_1 = (long)(m_1);
            }
            m_1 = (long)((long)(m_1) + 1L);
        }
        return knn.labels[(int)((long)(max_idx_1))];
    }
    public static void main(String[] args) {
        knn = make_knn(((double[][])(train_X)), ((long[])(train_y)), ((String[])(classes)));
        System.out.println(classify(knn, ((double[])(point)), 5L));
    }
}
