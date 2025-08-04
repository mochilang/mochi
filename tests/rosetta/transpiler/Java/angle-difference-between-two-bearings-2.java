public class Main {
    static double[][] testCases;

    static double angleDiff(double b1, double b2) {
        double diff = b2 - b1;
        return ((diff % 360.0 + 360.0 + 180.0) % 360.0) - 180.0;
    }
    public static void main(String[] args) {
        testCases = ((double[][])(new double[][]{new double[]{20.0, 45.0}, new double[]{0 - 45.0, 45.0}, new double[]{0 - 85.0, 90.0}, new double[]{0 - 95.0, 90.0}, new double[]{0 - 45.0, 125.0}, new double[]{0 - 45.0, 145.0}, new double[]{29.4803, 0 - 88.6381}, new double[]{0 - 78.3251, 0 - 159.036}, new double[]{0 - 70099.74233810938, 29840.67437876723}, new double[]{0 - 165313.6666297357, 33693.9894517456}, new double[]{1174.8380510598456, 0 - 154146.66490124757}, new double[]{60175.77306795546, 42213.07192354373}}));
        for (double[] tc : testCases) {
            System.out.println(angleDiff(tc[0], tc[1]));
        }
    }
}
