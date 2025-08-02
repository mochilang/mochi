public class Main {
    static class Point {
        double x;
        double y;
        Point(double x, double y) {
            this.x = x;
            this.y = y;
        }
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static String Two;
    static String R0;
    static String Co;
    static String CoR0;
    static String Diam;
    static String Far;
    static Object[][] td;

    static double sqrtApprox(double x) {
        double g = x;
        int i = 0;
        while (i < 40) {
            g = (g + x / g) / 2.0;
            i = i + 1;
        }
        return g;
    }

    static double hypot(double x, double y) {
        return sqrtApprox(x * x + y * y);
    }

    static Object[] circles(Point p1, Point p2, double r) {
        if (p1.x == p2.x && p1.y == p2.y) {
            if (r == 0.0) {
                return new Object[]{p1, p1, "Coincident points with r==0.0 describe a degenerate circle."};
            }
            return new Object[]{p1, p2, "Coincident points describe an infinite number of circles."};
        }
        if (r == 0.0) {
            return new Object[]{p1, p2, "R==0.0 does not describe circles."};
        }
        double dx = p2.x - p1.x;
        double dy = p2.y - p1.y;
        double q = hypot(dx, dy);
        if (q > 2.0 * r) {
            return new Object[]{p1, p2, "Points too far apart to form circles."};
        }
        Point m = new Point((p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0);
        if (q == 2.0 * r) {
            return new Object[]{m, m, "Points form a diameter and describe only a single circle."};
        }
        double d = sqrtApprox(r * r - q * q / 4.0);
        double ox = d * dx / q;
        double oy = d * dy / q;
        return new Object[]{new Point(m.x - oy, m.y + ox), new Point(m.x + oy, m.y - ox), "Two circles."};
    }
    public static void main(String[] args) {
        Two = "Two circles.";
        R0 = "R==0.0 does not describe circles.";
        Co = "Coincident points describe an infinite number of circles.";
        CoR0 = "Coincident points with r==0.0 describe a degenerate circle.";
        Diam = "Points form a diameter and describe only a single circle.";
        Far = "Points too far apart to form circles.";
        td = ((Object[][])(new Object[][]{new Object[]{new Point(0.1234, 0.9876), new Point(0.8765, 0.2345), 2.0}, new Object[]{new Point(0.0, 2.0), new Point(0.0, 0.0), 1.0}, new Object[]{new Point(0.1234, 0.9876), new Point(0.1234, 0.9876), 2.0}, new Object[]{new Point(0.1234, 0.9876), new Point(0.8765, 0.2345), 0.5}, new Object[]{new Point(0.1234, 0.9876), new Point(0.1234, 0.9876), 0.0}}));
        for (Object[] tc : td) {
            Object p1 = tc[0];
            Object p2 = tc[1];
            Object r = tc[2];
            System.out.println("p1:  {" + _p(((Point)p1).x) + " " + _p(((Point)p1).y) + "}");
            System.out.println("p2:  {" + _p(((Point)p2).x) + " " + _p(((Point)p2).y) + "}");
            System.out.println("r:  " + _p(r));
            Object[] res = ((Object[])(circles(p1, p2, ((Number)(r)).doubleValue())));
            Object c1 = res[0];
            Object c2 = res[1];
            Object caseStr = res[2];
            System.out.println("   " + (String)(caseStr));
            if (((Number)(caseStr)).intValue() == "Points form a diameter and describe only a single circle." || ((Number)(caseStr)).intValue() == "Coincident points with r==0.0 describe a degenerate circle.") {
                System.out.println("   Center:  {" + _p(((Point)c1).x) + " " + _p(((Point)c1).y) + "}");
            } else             if (((Number)(caseStr)).intValue() == "Two circles.") {
                System.out.println("   Center 1:  {" + _p(((Point)c1).x) + " " + _p(((Point)c1).y) + "}");
                System.out.println("   Center 2:  {" + _p(((Point)c2).x) + " " + _p(((Point)c2).y) + "}");
            }
            System.out.println("");
        }
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
