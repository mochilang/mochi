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

    static java.util.Map<String,Object> makeSym(int order, double[] elements) {
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("order", order), java.util.Map.entry("ele", elements)));
    }

    static double[][] unpackSym(java.util.Map<String,Object> m) {
        Object n = (Object)(((Object)m.get("order")));
        Object ele = (Object)(((Object)m.get("ele")));
        double[][] mat = new double[][]{};
        int idx = 0;
        int r = 0;
        while (r < ((Number)(n)).intValue()) {
            double[] row = new double[]{};
            int c = 0;
            while (c <= r) {
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(((Number)(ele[idx])).doubleValue())).toArray();
                idx = idx + 1;
                c = c + 1;
            }
            while (c < ((Number)(n)).intValue()) {
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray();
                c = c + 1;
            }
            mat = appendObj(mat, row);
            r = r + 1;
        }
        r = 0;
        while (r < ((Number)(n)).intValue()) {
            int c = r + 1;
            while (c < ((Number)(n)).intValue()) {
mat[r][c] = mat[c][r];
                c = c + 1;
            }
            r = r + 1;
        }
        return mat;
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

    static void printSym(java.util.Map<String,Object> m) {
        printMat(unpackSym(m));
    }

    static void printLower(java.util.Map<String,Object> m) {
        Object n = (Object)(((Object)m.get("order")));
        Object ele = (Object)(((Object)m.get("ele")));
        double[][] mat = new double[][]{};
        int idx = 0;
        int r = 0;
        while (r < ((Number)(n)).intValue()) {
            double[] row = new double[]{};
            int c = 0;
            while (c <= r) {
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(((Number)(ele[idx])).doubleValue())).toArray();
                idx = idx + 1;
                c = c + 1;
            }
            while (c < ((Number)(n)).intValue()) {
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray();
                c = c + 1;
            }
            mat = appendObj(mat, row);
            r = r + 1;
        }
        printMat(mat);
    }

    static java.util.Map<String,Object> choleskyLower(java.util.Map<String,Object> a) {
        Object n = (Object)(((Object)a.get("order")));
        Object ae = (Object)(((Object)a.get("ele")));
        double[] le = new double[]{};
        int idx = 0;
        while (idx < String.valueOf(ae).length()) {
            le = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(le), java.util.stream.DoubleStream.of(0.0)).toArray();
            idx = idx + 1;
        }
        int row = 1;
        int col = 1;
        int dr = 0;
        int dc = 0;
        int i = 0;
        while (i < String.valueOf(ae).length()) {
            Object e = ae[i];
            if (i < dr) {
                double d = (((Number)(e)).intValue() - le[i]) / le[dc];
le[i] = d;
                int ci = col;
                int cx = dc;
                int j = i + 1;
                while (j <= dr) {
                    cx = cx + ci;
                    ci = ci + 1;
le[j] = le[j] + d * le[cx];
                    j = j + 1;
                }
                col = col + 1;
                dc = dc + col;
            } else {
le[i] = sqrtApprox(((Number)(e)).intValue() - le[i]);
                row = row + 1;
                dr = dr + row;
                col = 1;
                dc = 0;
            }
            i = i + 1;
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("order", n), java.util.Map.entry("ele", le)));
    }

    static void demo(java.util.Map<String,Object> a) {
        System.out.println("A:");
        printSym(a);
        System.out.println("L:");
        java.util.Map<String,Object> l = choleskyLower(a);
        printLower(l);
    }
    public static void main(String[] args) {
        demo(makeSym(3, new double[]{25.0, 15.0, 18.0, -5.0, 0.0, 11.0}));
        demo(makeSym(4, new double[]{18.0, 22.0, 70.0, 54.0, 86.0, 174.0, 42.0, 62.0, 134.0, 106.0}));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
