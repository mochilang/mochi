public class Main {
    static double[] empty = ((double[])(new double[]{}));

    static double max_subarray_sum(double[] nums, boolean allow_empty) {
        if (new java.math.BigInteger(String.valueOf(nums.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return (double)(0.0);
        }
        double max_sum_1 = (double)(0.0);
        double curr_sum_1 = (double)(0.0);
        if (allow_empty) {
            max_sum_1 = (double)(0.0);
            curr_sum_1 = (double)(0.0);
            java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
            while (i_2.compareTo(new java.math.BigInteger(String.valueOf(nums.length))) < 0) {
                double num_2 = (double)(nums[_idx((nums).length, ((java.math.BigInteger)(i_2)).longValue())]);
                double temp_2 = (double)((double)(curr_sum_1) + (double)(num_2));
                curr_sum_1 = (double)((double)(temp_2) > (double)(0.0) ? temp_2 : 0.0);
                if ((double)(curr_sum_1) > (double)(max_sum_1)) {
                    max_sum_1 = (double)(curr_sum_1);
                }
                i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
            }
        } else {
            max_sum_1 = (double)(nums[_idx((nums).length, 0L)]);
            curr_sum_1 = (double)(nums[_idx((nums).length, 0L)]);
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
            while (i_3.compareTo(new java.math.BigInteger(String.valueOf(nums.length))) < 0) {
                double num_3 = (double)(nums[_idx((nums).length, ((java.math.BigInteger)(i_3)).longValue())]);
                double temp_3 = (double)((double)(curr_sum_1) + (double)(num_3));
                curr_sum_1 = (double)((double)(temp_3) > (double)(num_3) ? temp_3 : num_3);
                if ((double)(curr_sum_1) > (double)(max_sum_1)) {
                    max_sum_1 = (double)(curr_sum_1);
                }
                i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            }
        }
        return (double)(max_sum_1);
    }
    public static void main(String[] args) {
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(2.0), (double)(8.0), (double)(9.0)})), false)));
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(0.0), (double)(0.0)})), false)));
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(-1.0), (double)(0.0), (double)(1.0)})), false)));
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(1.0), (double)(2.0), (double)(3.0), (double)(4.0), (double)(-2.0)})), false)));
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(-2.0), (double)(1.0), (double)(-3.0), (double)(4.0), (double)(-1.0), (double)(2.0), (double)(1.0), (double)(-5.0), (double)(4.0)})), false)));
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(2.0), (double)(3.0), (double)(-9.0), (double)(8.0), (double)(-2.0)})), false)));
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(-2.0), (double)(-3.0), (double)(-1.0), (double)(-4.0), (double)(-6.0)})), false)));
        System.out.println(_p(max_subarray_sum(((double[])(new double[]{(double)(-2.0), (double)(-3.0), (double)(-1.0), (double)(-4.0), (double)(-6.0)})), true)));
        System.out.println(_p(max_subarray_sum(((double[])(empty)), false)));
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
