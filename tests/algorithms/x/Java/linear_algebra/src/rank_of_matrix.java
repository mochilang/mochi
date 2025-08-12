public class Main {
    static long rank_of_matrix(double[][] matrix) {
        long rows = matrix.length;
        if (rows == 0) {
            return 0;
        }
        long columns_1 = matrix[(int)((long)(0))].length > 0 ? matrix[(int)((long)(0))].length : 0;
        long rank_1 = rows < columns_1 ? rows : columns_1;
        long row_1 = 0L;
        while (row_1 < rank_1) {
            if (matrix[(int)((long)(row_1))][(int)((long)(row_1))] != 0.0) {
                long col_1 = row_1 + 1;
                while (col_1 < rows) {
                    double mult_1 = matrix[(int)((long)(col_1))][(int)((long)(row_1))] / matrix[(int)((long)(row_1))][(int)((long)(row_1))];
                    long i_2 = row_1;
                    while (i_2 < columns_1) {
matrix[(int)((long)(col_1))][(int)((long)(i_2))] = matrix[(int)((long)(col_1))][(int)((long)(i_2))] - mult_1 * matrix[(int)((long)(row_1))][(int)((long)(i_2))];
                        i_2 = i_2 + 1;
                    }
                    col_1 = col_1 + 1;
                }
            } else {
                boolean reduce_1 = true;
                long i_3 = row_1 + 1;
                while (i_3 < rows) {
                    if (matrix[(int)((long)(i_3))][(int)((long)(row_1))] != 0.0) {
                        double[] temp_1 = ((double[])(matrix[(int)((long)(row_1))]));
matrix[(int)((long)(row_1))] = ((double[])(matrix[(int)((long)(i_3))]));
matrix[(int)((long)(i_3))] = ((double[])(temp_1));
                        reduce_1 = false;
                        break;
                    }
                    i_3 = i_3 + 1;
                }
                if (reduce_1) {
                    rank_1 = rank_1 - 1;
                    long j_1 = 0L;
                    while (j_1 < rows) {
matrix[(int)((long)(j_1))][(int)((long)(row_1))] = matrix[(int)((long)(j_1))][(int)((long)(rank_1))];
                        j_1 = j_1 + 1;
                    }
                }
                row_1 = row_1 - 1;
            }
            row_1 = row_1 + 1;
        }
        return rank_1;
    }
    public static void main(String[] args) {
    }
}
