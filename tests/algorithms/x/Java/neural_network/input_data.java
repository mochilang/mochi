public class Main {
    static class DataSet {
        java.math.BigInteger[][] images;
        java.math.BigInteger[][] labels;
        java.math.BigInteger num_examples;
        java.math.BigInteger index_in_epoch;
        java.math.BigInteger epochs_completed;
        DataSet(java.math.BigInteger[][] images, java.math.BigInteger[][] labels, java.math.BigInteger num_examples, java.math.BigInteger index_in_epoch, java.math.BigInteger epochs_completed) {
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
        java.math.BigInteger[][] images;
        java.math.BigInteger[][] labels;
        BatchResult(DataSet dataset, java.math.BigInteger[][] images, java.math.BigInteger[][] labels) {
            this.dataset = dataset;
            this.images = images;
            this.labels = labels;
        }
        BatchResult() {}
        @Override public String toString() {
            return String.format("{'dataset': %s, 'images': %s, 'labels': %s}", String.valueOf(dataset), String.valueOf(images), String.valueOf(labels));
        }
    }


    static java.math.BigInteger[][] dense_to_one_hot(java.math.BigInteger[] labels, java.math.BigInteger num_classes) {
        java.math.BigInteger[][] result = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(labels.length))) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(num_classes) < 0) {
                if (j_1.compareTo(labels[_idx((labels).length, ((java.math.BigInteger)(i_1)).longValue())]) == 0) {
                    row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1))).toArray(java.math.BigInteger[]::new)));
                } else {
                    row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                }
                j_1 = j_1.add(java.math.BigInteger.valueOf(1));
            }
            result = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((java.math.BigInteger[][])(result));
    }

    static DataSet new_dataset(java.math.BigInteger[][] images, java.math.BigInteger[][] labels) {
        return new DataSet(((java.math.BigInteger[][])(images)), ((java.math.BigInteger[][])(labels)), new java.math.BigInteger(String.valueOf(images.length)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0));
    }

    static BatchResult next_batch(DataSet ds, java.math.BigInteger batch_size) {
        java.math.BigInteger start = ds.index_in_epoch;
        if (start.add(batch_size).compareTo(ds.num_examples) > 0) {
            java.math.BigInteger rest_1 = ds.num_examples.subtract(start);
            java.math.BigInteger[][] images_rest_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(ds.images, (int)(((java.math.BigInteger)(start)).longValue()), (int)(((java.math.BigInteger)(ds.num_examples)).longValue()))));
            java.math.BigInteger[][] labels_rest_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(ds.labels, (int)(((java.math.BigInteger)(start)).longValue()), (int)(((java.math.BigInteger)(ds.num_examples)).longValue()))));
            java.math.BigInteger new_index_1 = batch_size.subtract(rest_1);
            java.math.BigInteger[][] images_new_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(ds.images, (int)(0L), (int)(((java.math.BigInteger)(new_index_1)).longValue()))));
            java.math.BigInteger[][] labels_new_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(ds.labels, (int)(0L), (int)(((java.math.BigInteger)(new_index_1)).longValue()))));
            java.math.BigInteger[][] batch_images_2 = ((java.math.BigInteger[][])(concat(images_rest_1, images_new_1)));
            java.math.BigInteger[][] batch_labels_2 = ((java.math.BigInteger[][])(concat(labels_rest_1, labels_new_1)));
            DataSet new_ds_2 = new DataSet(((java.math.BigInteger[][])(ds.images)), ((java.math.BigInteger[][])(ds.labels)), ds.num_examples, new_index_1, ds.epochs_completed.add(java.math.BigInteger.valueOf(1)));
            return new BatchResult(new_ds_2, ((java.math.BigInteger[][])(batch_images_2)), ((java.math.BigInteger[][])(batch_labels_2)));
        } else {
            java.math.BigInteger end_1 = start.add(batch_size);
            java.math.BigInteger[][] batch_images_3 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(ds.images, (int)(((java.math.BigInteger)(start)).longValue()), (int)(((java.math.BigInteger)(end_1)).longValue()))));
            java.math.BigInteger[][] batch_labels_3 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(ds.labels, (int)(((java.math.BigInteger)(start)).longValue()), (int)(((java.math.BigInteger)(end_1)).longValue()))));
            DataSet new_ds_3 = new DataSet(((java.math.BigInteger[][])(ds.images)), ((java.math.BigInteger[][])(ds.labels)), ds.num_examples, end_1, ds.epochs_completed);
            return new BatchResult(new_ds_3, ((java.math.BigInteger[][])(batch_images_3)), ((java.math.BigInteger[][])(batch_labels_3)));
        }
    }

    static Datasets read_data_sets(java.math.BigInteger[][] train_images, java.math.BigInteger[] train_labels_raw, java.math.BigInteger[][] test_images, java.math.BigInteger[] test_labels_raw, java.math.BigInteger validation_size, java.math.BigInteger num_classes) {
        java.math.BigInteger[][] train_labels = ((java.math.BigInteger[][])(dense_to_one_hot(((java.math.BigInteger[])(train_labels_raw)), num_classes)));
        java.math.BigInteger[][] test_labels_1 = ((java.math.BigInteger[][])(dense_to_one_hot(((java.math.BigInteger[])(test_labels_raw)), num_classes)));
        java.math.BigInteger[][] validation_images_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(train_images, (int)(0L), (int)(((java.math.BigInteger)(validation_size)).longValue()))));
        java.math.BigInteger[][] validation_labels_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(train_labels, (int)(0L), (int)(((java.math.BigInteger)(validation_size)).longValue()))));
        java.math.BigInteger[][] train_images_rest_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(train_images, (int)(((java.math.BigInteger)(validation_size)).longValue()), (int)((long)(train_images.length)))));
        java.math.BigInteger[][] train_labels_rest_1 = ((java.math.BigInteger[][])(java.util.Arrays.copyOfRange(train_labels, (int)(((java.math.BigInteger)(validation_size)).longValue()), (int)((long)(train_labels.length)))));
        DataSet train_1 = new_dataset(((java.math.BigInteger[][])(train_images_rest_1)), ((java.math.BigInteger[][])(train_labels_rest_1)));
        DataSet validation_1 = new_dataset(((java.math.BigInteger[][])(validation_images_1)), ((java.math.BigInteger[][])(validation_labels_1)));
        DataSet testset_1 = new_dataset(((java.math.BigInteger[][])(test_images)), ((java.math.BigInteger[][])(test_labels_1)));
        return new Datasets(train_1, validation_1, testset_1);
    }

    static void main() {
        java.math.BigInteger[][] train_images = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)}))}));
        java.math.BigInteger[] train_labels_raw_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)}));
        java.math.BigInteger[][] test_images_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7)}))}));
        java.math.BigInteger[] test_labels_raw_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6)}));
        Datasets data_1 = read_data_sets(((java.math.BigInteger[][])(train_images)), ((java.math.BigInteger[])(train_labels_raw_1)), ((java.math.BigInteger[][])(test_images_1)), ((java.math.BigInteger[])(test_labels_raw_1)), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(10));
        DataSet ds_1 = data_1.train;
        BatchResult res_1 = next_batch(ds_1, java.math.BigInteger.valueOf(2));
        ds_1 = res_1.dataset;
        System.out.println(_p(res_1.images));
        System.out.println(_p(res_1.labels));
        res_1 = next_batch(ds_1, java.math.BigInteger.valueOf(2));
        ds_1 = res_1.dataset;
        System.out.println(_p(res_1.images));
        System.out.println(_p(res_1.labels));
        res_1 = next_batch(ds_1, java.math.BigInteger.valueOf(2));
        ds_1 = res_1.dataset;
        System.out.println(_p(res_1.images));
        System.out.println(_p(res_1.labels));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
