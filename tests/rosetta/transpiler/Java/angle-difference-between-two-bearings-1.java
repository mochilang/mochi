public class Main {
    static double[][] testCases;

    static double angleDiff(double b1, double b2) {
        double d = b2 - b1;
        if (d < 0 - 180.0) {
            return d + 360.0;
        }
        if (d > 180.0) {
            return d - 360.0;
        }
        return d;
    }
    public static void main(String[] args) {
        testCases = ((double[][])(new double[][]{new double[]{20.0, 45.0}, new double[]{0 - 45.0, 45.0}, new double[]{0 - 85.0, 90.0}, new double[]{0 - 95.0, 90.0}, new double[]{0 - 45.0, 125.0}, new double[]{0 - 45.0, 145.0}, new double[]{29.4803, 0 - 88.6381}, new double[]{0 - 78.3251, 0 - 159.036}}));
        for (double[] tc : testCases) {
            System.out.println(angleDiff(tc[0], tc[1]));
        }
    }
}
