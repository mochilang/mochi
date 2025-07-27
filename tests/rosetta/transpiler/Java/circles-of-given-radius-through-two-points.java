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

    static String Two = "Two circles.";
    static String R0 = "R==0.0 does not describe circles.";
    static String Co = "Coincident points describe an infinite number of circles.";
    static String CoR0 = "Coincident points with r==0.0 describe a degenerate circle.";
    static String Diam = "Points form a diameter and describe only a single circle.";
    static String Far = "Points too far apart to form circles.";
    static Point[][] td = new Point[][]{new Point[]{new Point(0.1234, 0.9876), new Point(0.8765, 0.2345), 2.0}, new Point[]{new Point(0.0, 2.0), new Point(0.0, 0.0), 1.0}, new Point[]{new Point(0.1234, 0.9876), new Point(0.1234, 0.9876), 2.0}, new Point[]{new Point(0.1234, 0.9876), new Point(0.8765, 0.2345), 0.5}, new Point[]{new Point(0.1234, 0.9876), new Point(0.1234, 0.9876), 0.0}};

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
                return new Point[]{p1, p1, "Coincident points with r==0.0 describe a degenerate circle."};
            }
            return new Point[]{p1, p2, "Coincident points describe an infinite number of circles."};
        }
        if (r == 0.0) {
            return new Point[]{p1, p2, "R==0.0 does not describe circles."};
        }
        double dx = p2.x - p1.x;
        double dy = p2.y - p1.y;
        double q = hypot(dx, dy);
        if (q > 2.0 * r) {
            return new Point[]{p1, p2, "Points too far apart to form circles."};
        }
        Point m = new Point((p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0);
        if (q == 2.0 * r) {
            return new Point[]{m, m, "Points form a diameter and describe only a single circle."};
        }
        double d = sqrtApprox(r * r - q * q / 4.0);
        double ox = d * dx / q;
        double oy = d * dy / q;
        return new Point[]{new Point(m.x - oy, m.y + ox), new Point(m.x + oy, m.y - ox), "Two circles."};
    }
    public static void main(String[] args) {
        for (Point[] tc : td) {
            Point p1 = tc[0];
            Point p2 = tc[1];
            Point r = tc[2];
            System.out.println("p1:  {" + String.valueOf(p1.x) + " " + String.valueOf(p1.y) + "}");
            System.out.println("p2:  {" + String.valueOf(p2.x) + " " + String.valueOf(p2.y) + "}");
            System.out.println("r:  " + String.valueOf(r));
            Object[] res = circles(p1, p2, r);
            Object c1 = res[0];
            Object c2 = res[1];
            Object caseStr = res[2];
            System.out.println("   " + (String)(caseStr));
            if (((Number)(caseStr)).intValue() == "Points form a diameter and describe only a single circle." || ((Number)(caseStr)).intValue() == "Coincident points with r==0.0 describe a degenerate circle.") {
                System.out.println("   Center:  {" + String.valueOf(c1.x) + " " + String.valueOf(c1.y) + "}");
            } else             if (((Number)(caseStr)).intValue() == "Two circles.") {
                System.out.println("   Center 1:  {" + String.valueOf(c1.x) + " " + String.valueOf(c1.y) + "}");
                System.out.println("   Center 2:  {" + String.valueOf(c2.x) + " " + String.valueOf(c2.y) + "}");
            }
            System.out.println("");
        }
    }
}
