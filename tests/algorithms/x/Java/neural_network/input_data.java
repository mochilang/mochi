public class Main {
    static class DataSet {
        long[][] images;
        long[][] labels;
        long num_examples;
        long index_in_epoch;
        long epochs_completed;
        DataSet(long[][] images, long[][] labels, long num_examples, long index_in_epoch, long epochs_completed) {
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
        long[][] images;
        long[][] labels;
        BatchResult(DataSet dataset, long[][] images, long[][] labels) {
            this.dataset = dataset;
            this.images = images;
            this.labels = labels;
        }
        BatchResult() {}
        @Override public String toString() {
            return String.format("{'dataset': %s, 'images': %s, 'labels': %s}", String.valueOf(dataset), String.valueOf(images), String.valueOf(labels));
        }
    }


    static long[][] dense_to_one_hot(long[] labels, long num_classes) {
        long[][] result = ((long[][])(new long[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(labels.length)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(num_classes)) {
                if ((long)(j_1) == (long)(labels[(int)((long)(i_1))])) {
                    row_1 = ((long[])(appendLong(row_1, 1L)));
                } else {
                    row_1 = ((long[])(appendLong(row_1, 0L)));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            result = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new long[][]{row_1})).toArray(long[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static DataSet new_dataset(long[][] images, long[][] labels) {
        return new DataSet(images, labels, images.length, 0, 0);
    }

    static BatchResult next_batch(DataSet ds, long batch_size) {
        long start = (long)(ds.index_in_epoch);
        if ((long)((long)(start) + (long)(batch_size)) > (long)(ds.num_examples)) {
            long rest_1 = (long)((long)(ds.num_examples) - (long)(start));
            long[][] images_rest_1 = ((long[][])(java.util.Arrays.copyOfRange(ds.images, (int)((long)(start)), (int)((long)(ds.num_examples)))));
            long[][] labels_rest_1 = ((long[][])(java.util.Arrays.copyOfRange(ds.labels, (int)((long)(start)), (int)((long)(ds.num_examples)))));
            long new_index_1 = (long)((long)(batch_size) - (long)(rest_1));
            long[][] images_new_1 = ((long[][])(java.util.Arrays.copyOfRange(ds.images, (int)(0L), (int)((long)(new_index_1)))));
            long[][] labels_new_1 = ((long[][])(java.util.Arrays.copyOfRange(ds.labels, (int)(0L), (int)((long)(new_index_1)))));
            long[][] batch_images_2 = ((long[][])(concat(images_rest_1, images_new_1)));
            long[][] batch_labels_2 = ((long[][])(concat(labels_rest_1, labels_new_1)));
            DataSet new_ds_2 = new DataSet(ds.images, ds.labels, ds.num_examples, new_index_1, (long)(ds.epochs_completed) + 1L);
            return new BatchResult(new_ds_2, batch_images_2, batch_labels_2);
        } else {
            long end_1 = (long)((long)(start) + (long)(batch_size));
            long[][] batch_images_3 = ((long[][])(java.util.Arrays.copyOfRange(ds.images, (int)((long)(start)), (int)((long)(end_1)))));
            long[][] batch_labels_3 = ((long[][])(java.util.Arrays.copyOfRange(ds.labels, (int)((long)(start)), (int)((long)(end_1)))));
            DataSet new_ds_3 = new DataSet(ds.images, ds.labels, ds.num_examples, end_1, ds.epochs_completed);
            return new BatchResult(new_ds_3, batch_images_3, batch_labels_3);
        }
    }

    static Datasets read_data_sets(long[][] train_images, long[] train_labels_raw, long[][] test_images, long[] test_labels_raw, long validation_size, long num_classes) {
        long[][] train_labels = ((long[][])(dense_to_one_hot(((long[])(train_labels_raw)), (long)(num_classes))));
        long[][] test_labels_1 = ((long[][])(dense_to_one_hot(((long[])(test_labels_raw)), (long)(num_classes))));
        long[][] validation_images_1 = ((long[][])(java.util.Arrays.copyOfRange(train_images, (int)(0L), (int)((long)(validation_size)))));
        long[][] validation_labels_1 = ((long[][])(java.util.Arrays.copyOfRange(train_labels, (int)(0L), (int)((long)(validation_size)))));
        long[][] train_images_rest_1 = ((long[][])(java.util.Arrays.copyOfRange(train_images, (int)((long)(validation_size)), (int)((long)(train_images.length)))));
        long[][] train_labels_rest_1 = ((long[][])(java.util.Arrays.copyOfRange(train_labels, (int)((long)(validation_size)), (int)((long)(train_labels.length)))));
        DataSet train_1 = new_dataset(((long[][])(train_images_rest_1)), ((long[][])(train_labels_rest_1)));
        DataSet validation_1 = new_dataset(((long[][])(validation_images_1)), ((long[][])(validation_labels_1)));
        DataSet testset_1 = new_dataset(((long[][])(test_images)), ((long[][])(test_labels_1)));
        return new Datasets(train_1, validation_1, testset_1);
    }

    static void main() {
        long[][] train_images = ((long[][])(new long[][]{new long[]{0, 1}, new long[]{1, 2}, new long[]{2, 3}, new long[]{3, 4}, new long[]{4, 5}}));
        long[] train_labels_raw_1 = ((long[])(new long[]{0, 1, 2, 3, 4}));
        long[][] test_images_1 = ((long[][])(new long[][]{new long[]{5, 6}, new long[]{6, 7}}));
        long[] test_labels_raw_1 = ((long[])(new long[]{5, 6}));
        Datasets data_1 = read_data_sets(((long[][])(train_images)), ((long[])(train_labels_raw_1)), ((long[][])(test_images_1)), ((long[])(test_labels_raw_1)), 2L, 10L);
        DataSet ds_1 = data_1.train;
        BatchResult res_1 = next_batch(ds_1, 2L);
        ds_1 = res_1.dataset;
        System.out.println(_p(res_1.images));
        System.out.println(_p(res_1.labels));
        res_1 = next_batch(ds_1, 2L);
        ds_1 = res_1.dataset;
        System.out.println(_p(res_1.images));
        System.out.println(_p(res_1.labels));
        res_1 = next_batch(ds_1, 2L);
        ds_1 = res_1.dataset;
        System.out.println(_p(res_1.images));
        System.out.println(_p(res_1.labels));
    }
    public static void main(String[] args) {
        main();
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static Object concat(Object a, Object b) {
        int len1 = java.lang.reflect.Array.getLength(a);
        int len2 = java.lang.reflect.Array.getLength(b);
        Object out = java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), len1 + len2);
        System.arraycopy(a, 0, out, 0, len1);
        System.arraycopy(b, 0, out, len1, len2);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
