public class Main {
    static class DataSet {
        int[][] images;
        int[][] labels;
        int num_examples;
        int index_in_epoch;
        int epochs_completed;
        DataSet(int[][] images, int[][] labels, int num_examples, int index_in_epoch, int epochs_completed) {
            this.images = images;
            this.labels = labels;
            this.num_examples = num_examples;
            this.index_in_epoch = index_in_epoch;
            this.epochs_completed = epochs_completed;
        }
        DataSet() {}
        @Override public String toString() {
            return String.format("{'images': %s, 'labels': %s, 'num_examples': %s, 'index_in_epoch': %s, 'epochs_completed': %s}", String.valueOf(images), String.valueOf(labels), String.valueOf(num_examples), String.valueOf(index_in_epoch), String.valueOf(epochs_completed));
        }
    }

    static class Datasets {
        DataSet train;
        DataSet validation;
        DataSet test_ds;
        Datasets(DataSet train, DataSet validation, DataSet test_ds) {
            this.train = train;
            this.validation = validation;
            this.test_ds = test_ds;
        }
        Datasets() {}
        @Override public String toString() {
            return String.format("{'train': %s, 'validation': %s, 'test_ds': %s}", String.valueOf(train), String.valueOf(validation), String.valueOf(test_ds));
        }
    }

    static class BatchResult {
        DataSet dataset;
        int[][] images;
        int[][] labels;
        BatchResult(DataSet dataset, int[][] images, int[][] labels) {
            this.dataset = dataset;
            this.images = images;
            this.labels = labels;
        }
        BatchResult() {}
        @Override public String toString() {
            return String.format("{'dataset': %s, 'images': %s, 'labels': %s}", String.valueOf(dataset), String.valueOf(images), String.valueOf(labels));
        }
    }


    static int[][] dense_to_one_hot(int[] labels, int num_classes) {
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < labels.length) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < num_classes) {
                if (j == labels[i]) {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(1)).toArray()));
                } else {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                }
                j = j + 1;
            }
            result = ((int[][])(appendObj(result, row)));
            i = i + 1;
        }
        return result;
    }

    static DataSet new_dataset(int[][] images, int[][] labels) {
        return new DataSet(images, labels, images.length, 0, 0);
    }

    static BatchResult next_batch(DataSet ds, int batch_size) {
        int start = ds.index_in_epoch;
        if (start + batch_size > ds.num_examples) {
            int rest = ds.num_examples - start;
            int[][] images_rest = ((int[][])(java.util.Arrays.copyOfRange(ds.images, start, ds.num_examples)));
            int[][] labels_rest = ((int[][])(java.util.Arrays.copyOfRange(ds.labels, start, ds.num_examples)));
            int new_index = batch_size - rest;
            int[][] images_new = ((int[][])(java.util.Arrays.copyOfRange(ds.images, 0, new_index)));
            int[][] labels_new = ((int[][])(java.util.Arrays.copyOfRange(ds.labels, 0, new_index)));
            Object batch_images = concat(images_rest, images_new);
            Object batch_labels = concat(labels_rest, labels_new);
            DataSet new_ds = new DataSet(ds.images, ds.labels, ds.num_examples, new_index, ds.epochs_completed + 1);
            return new BatchResult(new_ds, batch_images, batch_labels);
        } else {
            int end = start + batch_size;
            int[][] batch_images_1 = ((int[][])(java.util.Arrays.copyOfRange(ds.images, start, end)));
            int[][] batch_labels_1 = ((int[][])(java.util.Arrays.copyOfRange(ds.labels, start, end)));
            DataSet new_ds_1 = new DataSet(ds.images, ds.labels, ds.num_examples, end, ds.epochs_completed);
            return new BatchResult(new_ds_1, batch_images_1, batch_labels_1);
        }
    }

    static Datasets read_data_sets(int[][] train_images, int[] train_labels_raw, int[][] test_images, int[] test_labels_raw, int validation_size, int num_classes) {
        int[][] train_labels = ((int[][])(dense_to_one_hot(((int[])(train_labels_raw)), num_classes)));
        int[][] test_labels = ((int[][])(dense_to_one_hot(((int[])(test_labels_raw)), num_classes)));
        int[][] validation_images = ((int[][])(java.util.Arrays.copyOfRange(train_images, 0, validation_size)));
        int[][] validation_labels = ((int[][])(java.util.Arrays.copyOfRange(train_labels, 0, validation_size)));
        int[][] train_images_rest = ((int[][])(java.util.Arrays.copyOfRange(train_images, validation_size, train_images.length)));
        int[][] train_labels_rest = ((int[][])(java.util.Arrays.copyOfRange(train_labels, validation_size, train_labels.length)));
        DataSet train = new_dataset(((int[][])(train_images_rest)), ((int[][])(train_labels_rest)));
        DataSet validation = new_dataset(((int[][])(validation_images)), ((int[][])(validation_labels)));
        DataSet testset = new_dataset(((int[][])(test_images)), ((int[][])(test_labels)));
        return new Datasets(train, validation, testset);
    }

    static void main() {
        int[][] train_images = ((int[][])(new int[][]{new int[]{0, 1}, new int[]{1, 2}, new int[]{2, 3}, new int[]{3, 4}, new int[]{4, 5}}));
        int[] train_labels_raw = ((int[])(new int[]{0, 1, 2, 3, 4}));
        int[][] test_images = ((int[][])(new int[][]{new int[]{5, 6}, new int[]{6, 7}}));
        int[] test_labels_raw = ((int[])(new int[]{5, 6}));
        Datasets data = read_data_sets(((int[][])(train_images)), ((int[])(train_labels_raw)), ((int[][])(test_images)), ((int[])(test_labels_raw)), 2, 10);
        DataSet ds = data.train;
        BatchResult res = next_batch(ds, 2);
        ds = res.dataset;
        System.out.println(_p(res.images));
        System.out.println(_p(res.labels));
        res = next_batch(ds, 2);
        ds = res.dataset;
        System.out.println(_p(res.images));
        System.out.println(_p(res.labels));
        res = next_batch(ds, 2);
        ds = res.dataset;
        System.out.println(_p(res.images));
        System.out.println(_p(res.labels));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
