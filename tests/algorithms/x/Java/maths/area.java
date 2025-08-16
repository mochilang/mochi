public class Main {
    static double PI = (double)(3.141592653589793);
    static double TWO_PI = (double)(6.283185307179586);
    static double TRI_THREE_SIDES;

    static double _mod(double x, double m) {
        return (double)(x) - (double)((double)((((Number)(((Number)((double)(x) / (double)(m))).intValue())).doubleValue())) * (double)(m));
    }

    static double sin_approx(double x) {
        double y = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_1 = (double)((double)(y) * (double)(y));
        double y3_1 = (double)((double)(y2_1) * (double)(y));
        double y5_1 = (double)((double)(y3_1) * (double)(y2_1));
        double y7_1 = (double)((double)(y5_1) * (double)(y2_1));
        return (double)((double)((double)(y) - (double)((double)(y3_1) / (double)(6.0))) + (double)((double)(y5_1) / (double)(120.0))) - (double)((double)(y7_1) / (double)(5040.0));
    }

    static double cos_approx(double x) {
        double y_1 = (double)((double)(_mod((double)((double)(x) + (double)(PI)), (double)(TWO_PI))) - (double)(PI));
        double y2_3 = (double)((double)(y_1) * (double)(y_1));
        double y4_1 = (double)((double)(y2_3) * (double)(y2_3));
        double y6_1 = (double)((double)(y4_1) * (double)(y2_3));
        return (double)((double)((double)(1.0) - (double)((double)(y2_3) / (double)(2.0))) + (double)((double)(y4_1) / (double)(24.0))) - (double)((double)(y6_1) / (double)(720.0));
    }

    static double tan_approx(double x) {
        return (double)(sin_approx((double)(x))) / (double)(cos_approx((double)(x)));
    }

    static double sqrt_approx(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double surface_area_cube(double side_length) {
        if ((double)(side_length) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_cube() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)(6.0) * (double)(side_length)) * (double)(side_length);
    }

    static double surface_area_cuboid(double length, double breadth, double height) {
        if ((double)(length) < (double)(0.0) || (double)(breadth) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_cuboid() only accepts non-negative values");
            return 0.0;
        }
        return (double)(2.0) * (double)(((double)((double)(((double)(length) * (double)(breadth))) + (double)(((double)(breadth) * (double)(height)))) + (double)(((double)(length) * (double)(height)))));
    }

    static double surface_area_sphere(double radius) {
        if ((double)(radius) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_sphere() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)((double)(4.0) * (double)(PI)) * (double)(radius)) * (double)(radius);
    }

    static double surface_area_hemisphere(double radius) {
        if ((double)(radius) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_hemisphere() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)((double)(3.0) * (double)(PI)) * (double)(radius)) * (double)(radius);
    }

    static double surface_area_cone(double radius, double height) {
        if ((double)(radius) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_cone() only accepts non-negative values");
            return 0.0;
        }
        double slant_1 = (double)(sqrt_approx((double)((double)((double)(height) * (double)(height)) + (double)((double)(radius) * (double)(radius)))));
        return (double)((double)(PI) * (double)(radius)) * (double)(((double)(radius) + (double)(slant_1)));
    }

    static double surface_area_conical_frustum(double radius1, double radius2, double height) {
        if ((double)(radius1) < (double)(0.0) || (double)(radius2) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_conical_frustum() only accepts non-negative values");
            return 0.0;
        }
        double slant_3 = (double)(sqrt_approx((double)((double)((double)(height) * (double)(height)) + (double)((double)(((double)(radius1) - (double)(radius2))) * (double)(((double)(radius1) - (double)(radius2)))))));
        return (double)(PI) * (double)(((double)((double)((double)(slant_3) * (double)(((double)(radius1) + (double)(radius2)))) + (double)((double)(radius1) * (double)(radius1))) + (double)((double)(radius2) * (double)(radius2))));
    }

    static double surface_area_cylinder(double radius, double height) {
        if ((double)(radius) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_cylinder() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)((double)(2.0) * (double)(PI)) * (double)(radius)) * (double)(((double)(height) + (double)(radius)));
    }

    static double surface_area_torus(double torus_radius, double tube_radius) {
        if ((double)(torus_radius) < (double)(0.0) || (double)(tube_radius) < (double)(0.0)) {
            System.out.println("ValueError: surface_area_torus() only accepts non-negative values");
            return 0.0;
        }
        if ((double)(torus_radius) < (double)(tube_radius)) {
            System.out.println("ValueError: surface_area_torus() does not support spindle or self intersecting tori");
            return 0.0;
        }
        return (double)((double)((double)((double)(4.0) * (double)(PI)) * (double)(PI)) * (double)(torus_radius)) * (double)(tube_radius);
    }

    static double area_rectangle(double length, double width) {
        if ((double)(length) < (double)(0.0) || (double)(width) < (double)(0.0)) {
            System.out.println("ValueError: area_rectangle() only accepts non-negative values");
            return 0.0;
        }
        return (double)(length) * (double)(width);
    }

    static double area_square(double side_length) {
        if ((double)(side_length) < (double)(0.0)) {
            System.out.println("ValueError: area_square() only accepts non-negative values");
            return 0.0;
        }
        return (double)(side_length) * (double)(side_length);
    }

    static double area_triangle(double base, double height) {
        if ((double)(base) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            System.out.println("ValueError: area_triangle() only accepts non-negative values");
            return 0.0;
        }
        return (double)(((double)(base) * (double)(height))) / (double)(2.0);
    }

    static double area_triangle_three_sides(double side1, double side2, double side3) {
        if ((double)(side1) < (double)(0.0) || (double)(side2) < (double)(0.0) || (double)(side3) < (double)(0.0)) {
            System.out.println("ValueError: area_triangle_three_sides() only accepts non-negative values");
            return 0.0;
        }
        if ((double)((double)(side1) + (double)(side2)) < (double)(side3) || (double)((double)(side1) + (double)(side3)) < (double)(side2) || (double)((double)(side2) + (double)(side3)) < (double)(side1)) {
            System.out.println("ValueError: Given three sides do not form a triangle");
            return 0.0;
        }
        double s_1 = (double)((double)(((double)((double)(side1) + (double)(side2)) + (double)(side3))) / (double)(2.0));
        double prod_1 = (double)((double)((double)((double)(s_1) * (double)(((double)(s_1) - (double)(side1)))) * (double)(((double)(s_1) - (double)(side2)))) * (double)(((double)(s_1) - (double)(side3))));
        double res_1 = (double)(sqrt_approx((double)(prod_1)));
        return res_1;
    }

    static double area_parallelogram(double base, double height) {
        if ((double)(base) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            System.out.println("ValueError: area_parallelogram() only accepts non-negative values");
            return 0.0;
        }
        return (double)(base) * (double)(height);
    }

    static double area_trapezium(double base1, double base2, double height) {
        if ((double)(base1) < (double)(0.0) || (double)(base2) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            System.out.println("ValueError: area_trapezium() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)(0.5) * (double)(((double)(base1) + (double)(base2)))) * (double)(height);
    }

    static double area_circle(double radius) {
        if ((double)(radius) < (double)(0.0)) {
            System.out.println("ValueError: area_circle() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)(PI) * (double)(radius)) * (double)(radius);
    }

    static double area_ellipse(double radius_x, double radius_y) {
        if ((double)(radius_x) < (double)(0.0) || (double)(radius_y) < (double)(0.0)) {
            System.out.println("ValueError: area_ellipse() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)(PI) * (double)(radius_x)) * (double)(radius_y);
    }

    static double area_rhombus(double diagonal1, double diagonal2) {
        if ((double)(diagonal1) < (double)(0.0) || (double)(diagonal2) < (double)(0.0)) {
            System.out.println("ValueError: area_rhombus() only accepts non-negative values");
            return 0.0;
        }
        return (double)((double)(0.5) * (double)(diagonal1)) * (double)(diagonal2);
    }

    static double area_reg_polygon(long sides, double length) {
        if ((long)(sides) < 3L) {
            System.out.println("ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides");
            return 0.0;
        }
        if ((double)(length) < (double)(0.0)) {
            System.out.println("ValueError: area_reg_polygon() only accepts non-negative values as length of a side");
            return 0.0;
        }
        double n_1 = (double)(((Number)(sides)).doubleValue());
        return (double)(((double)((double)(n_1) * (double)(length)) * (double)(length))) / (double)(((double)(4.0) * (double)(tan_approx((double)((double)(PI) / (double)(n_1))))));
    }
    public static void main(String[] args) {
        System.out.println("[DEMO] Areas of various geometric shapes:");
        System.out.println("Rectangle: " + _p(area_rectangle((double)(10.0), (double)(20.0))));
        System.out.println("Square: " + _p(area_square((double)(10.0))));
        System.out.println("Triangle: " + _p(area_triangle((double)(10.0), (double)(10.0))));
        TRI_THREE_SIDES = (double)(area_triangle_three_sides((double)(5.0), (double)(12.0), (double)(13.0)));
        System.out.println("Triangle Three Sides: " + _p(TRI_THREE_SIDES));
        System.out.println("Parallelogram: " + _p(area_parallelogram((double)(10.0), (double)(20.0))));
        System.out.println("Rhombus: " + _p(area_rhombus((double)(10.0), (double)(20.0))));
        System.out.println("Trapezium: " + _p(area_trapezium((double)(10.0), (double)(20.0), (double)(30.0))));
        System.out.println("Circle: " + _p(area_circle((double)(20.0))));
        System.out.println("Ellipse: " + _p(area_ellipse((double)(10.0), (double)(20.0))));
        System.out.println("");
        System.out.println("Surface Areas of various geometric shapes:");
        System.out.println("Cube: " + _p(surface_area_cube((double)(20.0))));
        System.out.println("Cuboid: " + _p(surface_area_cuboid((double)(10.0), (double)(20.0), (double)(30.0))));
        System.out.println("Sphere: " + _p(surface_area_sphere((double)(20.0))));
        System.out.println("Hemisphere: " + _p(surface_area_hemisphere((double)(20.0))));
        System.out.println("Cone: " + _p(surface_area_cone((double)(10.0), (double)(20.0))));
        System.out.println("Conical Frustum: " + _p(surface_area_conical_frustum((double)(10.0), (double)(20.0), (double)(30.0))));
        System.out.println("Cylinder: " + _p(surface_area_cylinder((double)(10.0), (double)(20.0))));
        System.out.println("Torus: " + _p(surface_area_torus((double)(20.0), (double)(10.0))));
        System.out.println("Equilateral Triangle: " + _p(area_reg_polygon(3L, (double)(10.0))));
        System.out.println("Square: " + _p(area_reg_polygon(4L, (double)(10.0))));
        System.out.println("Regular Pentagon: " + _p(area_reg_polygon(5L, (double)(10.0))));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
