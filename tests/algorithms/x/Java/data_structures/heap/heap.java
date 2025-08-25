public class Main {
    static double[] heap = ((double[])(new double[]{(double)(103.0), (double)(9.0), (double)(1.0), (double)(7.0), (double)(11.0), (double)(15.0), (double)(25.0), (double)(201.0), (double)(209.0), (double)(107.0), (double)(5.0)}));
    static java.math.BigInteger size_1 = null;
    static double m;

    static java.math.BigInteger parent_index(java.math.BigInteger child_idx) {
        if (child_idx.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            return new java.math.BigInteger(String.valueOf((child_idx.subtract(java.math.BigInteger.valueOf(1))).divide(java.math.BigInteger.valueOf(2))));
        }
        return new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
    }

    static java.math.BigInteger left_child_idx(java.math.BigInteger parent_idx) {
        return new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(2).multiply(parent_idx).add(java.math.BigInteger.valueOf(1))));
    }

    static java.math.BigInteger right_child_idx(java.math.BigInteger parent_idx) {
        return new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(2).multiply(parent_idx).add(java.math.BigInteger.valueOf(2))));
    }

    static void max_heapify(double[] h, java.math.BigInteger heap_size, java.math.BigInteger index) {
        java.math.BigInteger largest = new java.math.BigInteger(String.valueOf(index));
        java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(left_child_idx(new java.math.BigInteger(String.valueOf(index)))));
        java.math.BigInteger right_1 = new java.math.BigInteger(String.valueOf(right_child_idx(new java.math.BigInteger(String.valueOf(index)))));
        if (left_1.compareTo(heap_size) < 0 && (double)(h[_idx((h).length, ((java.math.BigInteger)(left_1)).longValue())]) > (double)(h[_idx((h).length, ((java.math.BigInteger)(largest)).longValue())])) {
            largest = new java.math.BigInteger(String.valueOf(left_1));
        }
        if (right_1.compareTo(heap_size) < 0 && (double)(h[_idx((h).length, ((java.math.BigInteger)(right_1)).longValue())]) > (double)(h[_idx((h).length, ((java.math.BigInteger)(largest)).longValue())])) {
            largest = new java.math.BigInteger(String.valueOf(right_1));
        }
        if (largest.compareTo(index) != 0) {
            double temp_1 = (double)(h[_idx((h).length, ((java.math.BigInteger)(index)).longValue())]);
h[(int)(((java.math.BigInteger)(index)).longValue())] = (double)(h[_idx((h).length, ((java.math.BigInteger)(largest)).longValue())]);
h[(int)(((java.math.BigInteger)(largest)).longValue())] = (double)(temp_1);
            max_heapify(((double[])(h)), new java.math.BigInteger(String.valueOf(heap_size)), new java.math.BigInteger(String.valueOf(largest)));
        }
    }

    static java.math.BigInteger build_max_heap(double[] h) {
        java.math.BigInteger heap_size = new java.math.BigInteger(String.valueOf(h.length));
        java.math.BigInteger i_1 = new java.math.BigInteger(String.valueOf(heap_size.divide(java.math.BigInteger.valueOf(2)).subtract(java.math.BigInteger.valueOf(1))));
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            max_heapify(((double[])(h)), new java.math.BigInteger(String.valueOf(heap_size)), new java.math.BigInteger(String.valueOf(i_1)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
        }
        return new java.math.BigInteger(String.valueOf(heap_size));
    }

    static double extract_max(double[] h, java.math.BigInteger heap_size) {
        double max_value = (double)(h[_idx((h).length, 0L)]);
h[(int)(0L)] = (double)(h[_idx((h).length, ((java.math.BigInteger)(heap_size.subtract(java.math.BigInteger.valueOf(1)))).longValue())]);
        max_heapify(((double[])(h)), new java.math.BigInteger(String.valueOf(heap_size.subtract(java.math.BigInteger.valueOf(1)))), java.math.BigInteger.valueOf(0));
        return (double)(max_value);
    }

    static java.math.BigInteger insert(double[] h, java.math.BigInteger heap_size, double value) {
        if (heap_size.compareTo(new java.math.BigInteger(String.valueOf(h.length))) < 0) {
h[(int)(((java.math.BigInteger)(heap_size)).longValue())] = (double)(value);
        } else {
            h = ((double[])(appendDouble(h, (double)(value))));
        }
        heap_size = new java.math.BigInteger(String.valueOf(heap_size.add(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf((heap_size.subtract(java.math.BigInteger.valueOf(1))).divide(java.math.BigInteger.valueOf(2))));
        while (idx_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            max_heapify(((double[])(h)), new java.math.BigInteger(String.valueOf(heap_size)), new java.math.BigInteger(String.valueOf(idx_1)));
            idx_1 = new java.math.BigInteger(String.valueOf((idx_1.subtract(java.math.BigInteger.valueOf(1))).divide(java.math.BigInteger.valueOf(2))));
        }
        return new java.math.BigInteger(String.valueOf(heap_size));
    }

    static void heap_sort(double[] h, java.math.BigInteger heap_size) {
        java.math.BigInteger size = new java.math.BigInteger(String.valueOf(heap_size));
        java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(size.subtract(java.math.BigInteger.valueOf(1))));
        while (j_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            double temp_3 = (double)(h[_idx((h).length, 0L)]);
h[(int)(0L)] = (double)(h[_idx((h).length, ((java.math.BigInteger)(j_1)).longValue())]);
h[(int)(((java.math.BigInteger)(j_1)).longValue())] = (double)(temp_3);
            size = new java.math.BigInteger(String.valueOf(size.subtract(java.math.BigInteger.valueOf(1))));
            max_heapify(((double[])(h)), new java.math.BigInteger(String.valueOf(size)), java.math.BigInteger.valueOf(0));
            j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
        }
    }

    static String heap_to_string(double[] h, java.math.BigInteger heap_size) {
        String s = "[";
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(heap_size) < 0) {
            s = s + _p(_getd(h, ((Number)(i_3)).intValue()));
            if (i_3.compareTo(heap_size.subtract(java.math.BigInteger.valueOf(1))) < 0) {
                s = s + ", ";
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        size_1 = new java.math.BigInteger(String.valueOf(build_max_heap(((double[])(heap)))));
        System.out.println(heap_to_string(((double[])(heap)), new java.math.BigInteger(String.valueOf(size_1))));
        m = (double)(extract_max(((double[])(heap)), new java.math.BigInteger(String.valueOf(size_1))));
        size_1 = new java.math.BigInteger(String.valueOf(size_1.subtract(java.math.BigInteger.valueOf(1))));
        System.out.println(_p(m));
        System.out.println(heap_to_string(((double[])(heap)), new java.math.BigInteger(String.valueOf(size_1))));
        size_1 = new java.math.BigInteger(String.valueOf(insert(((double[])(heap)), new java.math.BigInteger(String.valueOf(size_1)), (double)(100.0))));
        System.out.println(heap_to_string(((double[])(heap)), new java.math.BigInteger(String.valueOf(size_1))));
        heap_sort(((double[])(heap)), new java.math.BigInteger(String.valueOf(size_1)));
        System.out.println(heap_to_string(((double[])(heap)), new java.math.BigInteger(String.valueOf(size_1))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
