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

    static class Line {
        double slope;
        double yint;
        Line(double slope, double yint) {
            this.slope = slope;
            this.yint = yint;
        }
        @Override public String toString() {
            return String.format("{'slope': %s, 'yint': %s}", String.valueOf(slope), String.valueOf(yint));
        }
    }


    static Line createLine(Point a, Point b) {
        double slope = (b.y - a.y) / (b.x - a.x);
        double yint = a.y - slope * a.x;
        return new Line(slope, yint);
    }

    static double evalX(Line l, double x) {
        return l.slope * x + l.yint;
    }

    static Point intersection(Line l1, Line l2) {
        if (l1.slope == l2.slope) {
            return new Point(0.0, 0.0);
        }
        double x = (l2.yint - l1.yint) / (l1.slope - l2.slope);
        double y = evalX(l1, x);
        return new Point(x, y);
    }

    static void main() {
        Line l1 = createLine(new Point(4.0, 0.0), new Point(6.0, 10.0));
        Line l2 = createLine(new Point(0.0, 3.0), new Point(10.0, 7.0));
        Point p = intersection(l1, l2);
        System.out.println("{" + String.valueOf(p.x) + " " + String.valueOf(p.y) + "}");
    }
    public static void main(String[] args) {
        main();
    }
}
