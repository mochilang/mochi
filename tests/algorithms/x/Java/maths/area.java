public class Main {
    static double PI;
    static double TWO_PI;
    static double TRI_THREE_SIDES;

    static double _mod(double x, double m) {
        return x - (((Number)(((Number)(x / m)).intValue())).doubleValue()) * m;
    }

    static double sin_approx(double x) {
        double y = _mod(x + PI, TWO_PI) - PI;
        double y2_1 = y * y;
        double y3_1 = y2_1 * y;
        double y5_1 = y3_1 * y2_1;
        double y7_1 = y5_1 * y2_1;
        return y - y3_1 / 6.0 + y5_1 / 120.0 - y7_1 / 5040.0;
    }

    static double cos_approx(double x) {
        double y_1 = _mod(x + PI, TWO_PI) - PI;
        double y2_3 = y_1 * y_1;
        double y4_1 = y2_3 * y2_3;
        double y6_1 = y4_1 * y2_3;
        return 1.0 - y2_3 / 2.0 + y4_1 / 24.0 - y6_1 / 720.0;
    }

    static double tan_approx(double x) {
        return sin_approx(x) / cos_approx(x);
    }

    static double sqrt_approx(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x / 2.0;
        long i_1 = 0;
        while (i_1 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double surface_area_cube(double side_length) {
        if (side_length < 0.0) {
            System.out.println("ValueError: surface_area_cube() only accepts non-negative values");
            return 0.0;
        }
        return 6.0 * side_length * side_length;
    }

    static double surface_area_cuboid(double length, double breadth, double height) {
        if (length < 0.0 || breadth < 0.0 || height < 0.0) {
            System.out.println("ValueError: surface_area_cuboid() only accepts non-negative values");
            return 0.0;
        }
        return 2.0 * ((length * breadth) + (breadth * height) + (length * height));
    }

    static double surface_area_sphere(double radius) {
        if (radius < 0.0) {
            System.out.println("ValueError: surface_area_sphere() only accepts non-negative values");
            return 0.0;
        }
        return 4.0 * PI * radius * radius;
    }

    static double surface_area_hemisphere(double radius) {
        if (radius < 0.0) {
            System.out.println("ValueError: surface_area_hemisphere() only accepts non-negative values");
            return 0.0;
        }
        return 3.0 * PI * radius * radius;
    }

    static double surface_area_cone(double radius, double height) {
        if (radius < 0.0 || height < 0.0) {
            System.out.println("ValueError: surface_area_cone() only accepts non-negative values");
            return 0.0;
        }
        double slant_1 = sqrt_approx(height * height + radius * radius);
        return PI * radius * (radius + slant_1);
    }

    static double surface_area_conical_frustum(double radius1, double radius2, double height) {
        if (radius1 < 0.0 || radius2 < 0.0 || height < 0.0) {
            System.out.println("ValueError: surface_area_conical_frustum() only accepts non-negative values");
            return 0.0;
        }
        double slant_3 = sqrt_approx(height * height + (radius1 - radius2) * (radius1 - radius2));
        return PI * (slant_3 * (radius1 + radius2) + radius1 * radius1 + radius2 * radius2);
    }

    static double surface_area_cylinder(double radius, double height) {
        if (radius < 0.0 || height < 0.0) {
            System.out.println("ValueError: surface_area_cylinder() only accepts non-negative values");
            return 0.0;
        }
        return 2.0 * PI * radius * (height + radius);
    }

    static double surface_area_torus(double torus_radius, double tube_radius) {
        if (torus_radius < 0.0 || tube_radius < 0.0) {
            System.out.println("ValueError: surface_area_torus() only accepts non-negative values");
            return 0.0;
        }
        if (torus_radius < tube_radius) {
            System.out.println("ValueError: surface_area_torus() does not support spindle or self intersecting tori");
            return 0.0;
        }
        return 4.0 * PI * PI * torus_radius * tube_radius;
    }

    static double area_rectangle(double length, double width) {
        if (length < 0.0 || width < 0.0) {
            System.out.println("ValueError: area_rectangle() only accepts non-negative values");
            return 0.0;
        }
        return length * width;
    }

    static double area_square(double side_length) {
        if (side_length < 0.0) {
            System.out.println("ValueError: area_square() only accepts non-negative values");
            return 0.0;
        }
        return side_length * side_length;
    }

    static double area_triangle(double base, double height) {
        if (base < 0.0 || height < 0.0) {
            System.out.println("ValueError: area_triangle() only accepts non-negative values");
            return 0.0;
        }
        return (base * height) / 2.0;
    }

    static double area_triangle_three_sides(double side1, double side2, double side3) {
        if (side1 < 0.0 || side2 < 0.0 || side3 < 0.0) {
            System.out.println("ValueError: area_triangle_three_sides() only accepts non-negative values");
            return 0.0;
        }
        if (side1 + side2 < side3 || side1 + side3 < side2 || side2 + side3 < side1) {
            System.out.println("ValueError: Given three sides do not form a triangle");
            return 0.0;
        }
        double s_1 = (side1 + side2 + side3) / 2.0;
        double prod_1 = s_1 * (s_1 - side1) * (s_1 - side2) * (s_1 - side3);
        double res_1 = sqrt_approx(prod_1);
        return res_1;
    }

    static double area_parallelogram(double base, double height) {
        if (base < 0.0 || height < 0.0) {
            System.out.println("ValueError: area_parallelogram() only accepts non-negative values");
            return 0.0;
        }
        return base * height;
    }

    static double area_trapezium(double base1, double base2, double height) {
        if (base1 < 0.0 || base2 < 0.0 || height < 0.0) {
            System.out.println("ValueError: area_trapezium() only accepts non-negative values");
            return 0.0;
        }
        return 0.5 * (base1 + base2) * height;
    }

    static double area_circle(double radius) {
        if (radius < 0.0) {
            System.out.println("ValueError: area_circle() only accepts non-negative values");
            return 0.0;
        }
        return PI * radius * radius;
    }

    static double area_ellipse(double radius_x, double radius_y) {
        if (radius_x < 0.0 || radius_y < 0.0) {
            System.out.println("ValueError: area_ellipse() only accepts non-negative values");
            return 0.0;
        }
        return PI * radius_x * radius_y;
    }

    static double area_rhombus(double diagonal1, double diagonal2) {
        if (diagonal1 < 0.0 || diagonal2 < 0.0) {
            System.out.println("ValueError: area_rhombus() only accepts non-negative values");
            return 0.0;
        }
        return 0.5 * diagonal1 * diagonal2;
    }

    static double area_reg_polygon(long sides, double length) {
        if (sides < 3) {
            System.out.println("ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides");
            return 0.0;
        }
        if (length < 0.0) {
            System.out.println("ValueError: area_reg_polygon() only accepts non-negative values as length of a side");
            return 0.0;
        }
        double n_1 = ((Number)(sides)).doubleValue();
        return (n_1 * length * length) / (4.0 * tan_approx(PI / n_1));
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        TWO_PI = 6.283185307179586;
        System.out.println("[DEMO] Areas of various geometric shapes:");
        System.out.println("Rectangle: " + _p(area_rectangle(10.0, 20.0)));
        System.out.println("Square: " + _p(area_square(10.0)));
        System.out.println("Triangle: " + _p(area_triangle(10.0, 10.0)));
        TRI_THREE_SIDES = area_triangle_three_sides(5.0, 12.0, 13.0);
        System.out.println("Triangle Three Sides: " + _p(TRI_THREE_SIDES));
        System.out.println("Parallelogram: " + _p(area_parallelogram(10.0, 20.0)));
        System.out.println("Rhombus: " + _p(area_rhombus(10.0, 20.0)));
        System.out.println("Trapezium: " + _p(area_trapezium(10.0, 20.0, 30.0)));
        System.out.println("Circle: " + _p(area_circle(20.0)));
        System.out.println("Ellipse: " + _p(area_ellipse(10.0, 20.0)));
        System.out.println("");
        System.out.println("Surface Areas of various geometric shapes:");
        System.out.println("Cube: " + _p(surface_area_cube(20.0)));
        System.out.println("Cuboid: " + _p(surface_area_cuboid(10.0, 20.0, 30.0)));
        System.out.println("Sphere: " + _p(surface_area_sphere(20.0)));
        System.out.println("Hemisphere: " + _p(surface_area_hemisphere(20.0)));
        System.out.println("Cone: " + _p(surface_area_cone(10.0, 20.0)));
        System.out.println("Conical Frustum: " + _p(surface_area_conical_frustum(10.0, 20.0, 30.0)));
        System.out.println("Cylinder: " + _p(surface_area_cylinder(10.0, 20.0)));
        System.out.println("Torus: " + _p(surface_area_torus(20.0, 10.0)));
        System.out.println("Equilateral Triangle: " + _p(area_reg_polygon(3, 10.0)));
        System.out.println("Square: " + _p(area_reg_polygon(4, 10.0)));
        System.out.println("Regular Pentagon: " + _p(area_reg_polygon(5, 10.0)));
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
