public class Main {

    static boolean[][] create_bool_matrix(java.math.BigInteger rows, java.math.BigInteger cols) {
        boolean[][] matrix = ((boolean[][])(new boolean[][]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(rows) <= 0) {
            boolean[] row_1 = ((boolean[])(new boolean[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(cols) <= 0) {
                row_1 = ((boolean[])(appendBool(row_1, false)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            matrix = ((boolean[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(matrix), java.util.stream.Stream.of(new boolean[][]{((boolean[])(row_1))})).toArray(boolean[][]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((boolean[][])(matrix));
    }

    static boolean is_sum_subset(java.math.BigInteger[] arr, java.math.BigInteger required_sum) {
        java.math.BigInteger arr_len = new java.math.BigInteger(String.valueOf(arr.length));
        boolean[][] subset_1 = ((boolean[][])(create_bool_matrix(new java.math.BigInteger(String.valueOf(arr_len)), new java.math.BigInteger(String.valueOf(required_sum)))));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(arr_len) <= 0) {
subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(0L)] = true;
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger j_3 = java.math.BigInteger.valueOf(1);
        while (j_3.compareTo(required_sum) <= 0) {
subset_1[_idx((subset_1).length, 0L)][(int)(((java.math.BigInteger)(j_3)).longValue())] = false;
            j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
        }
        i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(arr_len) <= 0) {
            j_3 = java.math.BigInteger.valueOf(1);
            while (j_3.compareTo(required_sum) <= 0) {
                if (arr[_idx((arr).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())].compareTo(j_3) > 0) {
subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())];
                }
                if (arr[_idx((arr).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())].compareTo(j_3) <= 0) {
subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_3)).longValue())] = subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())] || subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())][_idx((subset_1[_idx((subset_1).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_3.subtract(arr[_idx((arr).length, ((java.math.BigInteger)(i_3.subtract(java.math.BigInteger.valueOf(1)))).longValue())]))).longValue())];
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return subset_1[_idx((subset_1).length, ((java.math.BigInteger)(arr_len)).longValue())][_idx((subset_1[_idx((subset_1).length, ((java.math.BigInteger)(arr_len)).longValue())]).length, ((java.math.BigInteger)(required_sum)).longValue())];
    }
    public static void main(String[] args) {
        System.out.println(is_sum_subset(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(8)})), java.math.BigInteger.valueOf(5)));
        System.out.println(is_sum_subset(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(8)})), java.math.BigInteger.valueOf(14)));
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
