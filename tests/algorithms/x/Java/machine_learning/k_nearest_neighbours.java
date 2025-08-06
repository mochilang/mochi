public class Main {
    static class PointLabel {
        double[] point;
        int label;
        PointLabel(double[] point, int label) {
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
        int label;
        DistLabel(double dist, int label) {
            this.dist = dist;
            this.label = label;
        }
        DistLabel() {}
        @Override public String toString() {
            return String.format("{'dist': %s, 'label': %s}", String.valueOf(dist), String.valueOf(label));
        }
    }

    static double[][] train_X;
    static int[] train_y;
    static String[] classes;
    static KNN knn;
    static double[] point;

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static KNN make_knn(double[][] train_data, int[] train_target, String[] class_labels) {
        PointLabel[] items = ((PointLabel[])(new PointLabel[]{}));
        int i_1 = 0;
        while (i_1 < train_data.length) {
            PointLabel pl = new PointLabel(train_data[i_1], train_target[i_1]);
            items = ((PointLabel[])(java.util.stream.Stream.concat(java.util.Arrays.stream(items), java.util.stream.Stream.of(pl)).toArray(PointLabel[]::new)));
            i_1 = i_1 + 1;
        }
        return new KNN(items, class_labels);
    }

    static double euclidean_distance(double[] a, double[] b) {
        double sum = 0.0;
        int i_2 = 0;
        while (i_2 < a.length) {
            double diff = a[i_2] - b[i_2];
            sum = sum + diff * diff;
            i_2 = i_2 + 1;
        }
        return sqrtApprox(sum);
    }

    static String classify(KNN knn, double[] pred_point, int k) {
        DistLabel[] distances = ((DistLabel[])(new DistLabel[]{}));
        int i_3 = 0;
        while (i_3 < knn.data.length) {
            double d = euclidean_distance(((double[])(knn.data[i_3].point)), ((double[])(pred_point)));
            distances = ((DistLabel[])(java.util.stream.Stream.concat(java.util.Arrays.stream(distances), java.util.stream.Stream.of(new DistLabel(d, knn.data[i_3].label))).toArray(DistLabel[]::new)));
            i_3 = i_3 + 1;
        }
        int[] votes = ((int[])(new int[]{}));
        int count = 0;
        while (count < k) {
            int min_index = 0;
            int j = 1;
            while (j < distances.length) {
                if (distances[j].dist < distances[min_index].dist) {
                    min_index = j;
                }
                j = j + 1;
            }
            votes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(votes), java.util.stream.IntStream.of(distances[min_index].label)).toArray()));
            e18;
            count = count + 1;
        }
        int[] tally = ((int[])(new int[]{}));
        int t = 0;
        while (t < knn.labels.length) {
            tally = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(tally), java.util.stream.IntStream.of(0)).toArray()));
            t = t + 1;
        }
        int v = 0;
        while (v < votes.length) {
            int lbl = votes[v];
tally[lbl] = tally[lbl] + 1;
            v = v + 1;
        }
        int max_idx = 0;
        int m = 1;
        while (m < tally.length) {
            if (tally[m] > tally[max_idx]) {
                max_idx = m;
            }
            m = m + 1;
        }
        return knn.labels[max_idx];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            train_X = ((double[][])(new double[][]{new double[]{0.0, 0.0}, new double[]{1.0, 0.0}, new double[]{0.0, 1.0}, new double[]{0.5, 0.5}, new double[]{3.0, 3.0}, new double[]{2.0, 3.0}, new double[]{3.0, 2.0}}));
            train_y = ((int[])(new int[]{0, 0, 0, 0, 1, 1, 1}));
            classes = ((String[])(new String[]{"A", "B"}));
            knn = make_knn(((double[][])(train_X)), ((int[])(train_y)), ((String[])(classes)));
            point = ((double[])(new double[]{1.2, 1.2}));
            System.out.println(classify(knn, ((double[])(point)), 5));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
