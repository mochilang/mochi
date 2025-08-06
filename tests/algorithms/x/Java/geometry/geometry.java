public class Main {
    static double PI;
    static class Angle {
        double degrees;
        Angle(double degrees) {
            this.degrees = degrees;
        }
        Angle() {}
        @Override public String toString() {
            return String.format("{'degrees': %s}", String.valueOf(degrees));
        }
    }

    static class Side {
        double length;
        Angle angle;
        int next;
        Side(double length, Angle angle, int next) {
            this.length = length;
            this.angle = angle;
            this.next = next;
        }
        Side() {}
        @Override public String toString() {
            return String.format("{'length': %s, 'angle': %s, 'next': %s}", String.valueOf(length), String.valueOf(angle), String.valueOf(next));
        }
    }

    static class Ellipse {
        double major;
        double minor;
        Ellipse(double major, double minor) {
            this.major = major;
            this.minor = minor;
        }
        Ellipse() {}
        @Override public String toString() {
            return String.format("{'major': %s, 'minor': %s}", String.valueOf(major), String.valueOf(minor));
        }
    }

    static class Circle {
        double radius;
        Circle(double radius) {
            this.radius = radius;
        }
        Circle() {}
        @Override public String toString() {
            return String.format("{'radius': %s}", String.valueOf(radius));
        }
    }

    static class Polygon {
        Side[] sides;
        Polygon(Side[] sides) {
            this.sides = sides;
        }
        Polygon() {}
        @Override public String toString() {
            return String.format("{'sides': %s}", String.valueOf(sides));
        }
    }

    static class Rectangle {
        Side short_side;
        Side long_side;
        Polygon poly;
        Rectangle(Side short_side, Side long_side, Polygon poly) {
            this.short_side = short_side;
            this.long_side = long_side;
            this.poly = poly;
        }
        Rectangle() {}
        @Override public String toString() {
            return String.format("{'short_side': %s, 'long_side': %s, 'poly': %s}", String.valueOf(short_side), String.valueOf(long_side), String.valueOf(poly));
        }
    }

    static class Square {
        Side side;
        Rectangle rect;
        Square(Side side, Rectangle rect) {
            this.side = side;
            this.rect = rect;
        }
        Square() {}
        @Override public String toString() {
            return String.format("{'side': %s, 'rect': %s}", String.valueOf(side), String.valueOf(rect));
        }
    }


    static Angle make_angle(double deg) {
        if (deg < 0.0 || deg > 360.0) {
            throw new RuntimeException(String.valueOf("degrees must be between 0 and 360"));
        }
        return new Angle(deg);
    }

    static Side make_side(double length, Angle angle) {
        if (length <= 0.0) {
            throw new RuntimeException(String.valueOf("length must be positive"));
        }
        return new Side(length, angle, -1);
    }

    static double ellipse_area(Ellipse e) {
        return PI * e.major * e.minor;
    }

    static double ellipse_perimeter(Ellipse e) {
        return PI * (e.major + e.minor);
    }

    static double circle_area(Circle c) {
        Ellipse e = new Ellipse(c.radius, c.radius);
        double area = ellipse_area(e);
        return area;
    }

    static double circle_perimeter(Circle c) {
        Ellipse e_1 = new Ellipse(c.radius, c.radius);
        double per = ellipse_perimeter(e_1);
        return per;
    }

    static double circle_diameter(Circle c) {
        return c.radius * 2.0;
    }

    static double circle_max_parts(double num_cuts) {
        if (num_cuts < 0.0) {
            throw new RuntimeException(String.valueOf("num_cuts must be positive"));
        }
        return (num_cuts + 2.0 + num_cuts * num_cuts) * 0.5;
    }

    static Polygon make_polygon() {
        Side[] s = ((Side[])(new Side[]{}));
        return new Polygon(s);
    }

    static void polygon_add_side(Polygon p, Side s) {
p.sides = java.util.stream.Stream.concat(java.util.Arrays.stream(p.sides), java.util.stream.Stream.of(s)).toArray(Side[]::new);
    }

    static Side polygon_get_side(Polygon p, int index) {
        return p.sides[index];
    }

    static void polygon_set_side(Polygon p, int index, Side s) {
        Side[] tmp = ((Side[])(p.sides));
tmp[index] = s;
p.sides = tmp;
    }

    static Rectangle make_rectangle(double short_len, double long_len) {
        if (short_len <= 0.0 || long_len <= 0.0) {
            throw new RuntimeException(String.valueOf("length must be positive"));
        }
        Side short_ = make_side(short_len, make_angle(90.0));
        Side long_ = make_side(long_len, make_angle(90.0));
        Polygon p = make_polygon();
        polygon_add_side(p, short_);
        polygon_add_side(p, long_);
        return new Rectangle(short_, long_, p);
    }

    static double rectangle_perimeter(Rectangle r) {
        return (r.short_side.length + r.long_side.length) * 2.0;
    }

    static double rectangle_area(Rectangle r) {
        return r.short_side.length * r.long_side.length;
    }

    static Square make_square(double side_len) {
        Rectangle rect = make_rectangle(side_len, side_len);
        return new Square(rect.short_side, rect);
    }

    static double square_perimeter(Square s) {
        double p_1 = rectangle_perimeter(s.rect);
        return p_1;
    }

    static double square_area(Square s) {
        double a = rectangle_area(s.rect);
        return a;
    }

    static void main() {
        Angle a_1 = make_angle(90.0);
        System.out.println(a_1.degrees);
        Side s_1 = make_side(5.0, a_1);
        System.out.println(s_1.length);
        Ellipse e_2 = new Ellipse(5.0, 10.0);
        System.out.println(ellipse_area(e_2));
        System.out.println(ellipse_perimeter(e_2));
        Circle c = new Circle(5.0);
        System.out.println(circle_area(c));
        System.out.println(circle_perimeter(c));
        System.out.println(circle_diameter(c));
        System.out.println(circle_max_parts(7.0));
        Rectangle r = make_rectangle(5.0, 10.0);
        System.out.println(rectangle_perimeter(r));
        System.out.println(rectangle_area(r));
        Square q = make_square(5.0);
        System.out.println(square_perimeter(q));
        System.out.println(square_area(q));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
