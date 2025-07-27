public class Main {

    static double sqrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double[][] cholesky(double[][] a) {
        int n = a.length;
        double[][] l = new double[][]{};
        int i = 0;
        while (i < n) {
            double[] row = new double[]{};
            int j = 0;
            while (j < n) {
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray();
                j = j + 1;
            }
            l = appendObj(l, row);
            i = i + 1;
        }
        i = 0;
        while (i < n) {
            int j = 0;
            while (j <= i) {
                double sum = a[i][j];
                int k = 0;
                while (k < j) {
                    sum = sum - l[i][k] * l[j][k];
                    k = k + 1;
                }
                if (i == j) {
l[i][j] = sqrtApprox(sum);
                } else {
l[i][j] = sum / l[j][j];
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return l;
    }

    static void printMat(double[][] m) {
        int i = 0;
        while (i < m.length) {
            String line = "";
            int j = 0;
            while (j < m[i].length) {
                line = line + String.valueOf(m[i][j]);
                if (j < m[i].length - 1) {
                    line = line + " ";
                }
                j = j + 1;
            }
            System.out.println(line);
            i = i + 1;
        }
    }

    static void demo(double[][] a) {
        System.out.println("A:");
        printMat(a);
        double[][] l = cholesky(a);
        System.out.println("L:");
        printMat(l);
    }
    public static void main(String[] args) {
        demo(new double[][]{new double[]{25.0, 15.0, -5.0}, new double[]{15.0, 18.0, 0.0}, new double[]{-5.0, 0.0, 11.0}});
        demo(new double[][]{new double[]{18.0, 22.0, 54.0, 42.0}, new double[]{22.0, 70.0, 86.0, 62.0}, new double[]{54.0, 86.0, 174.0, 134.0}, new double[]{42.0, 62.0, 134.0, 106.0}});
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
