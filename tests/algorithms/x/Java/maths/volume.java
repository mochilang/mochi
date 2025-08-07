public class Main {
    static double PI;
    static double SQRT5;

    static double minf(double a, double b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static double maxf(double a, double b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static double vol_cube(double side_length) {
        if (side_length < 0.0) {
            throw new RuntimeException(String.valueOf("vol_cube() only accepts non-negative values"));
        }
        return side_length * side_length * side_length;
    }

    static double vol_spherical_cap(double height, double radius) {
        if (height < 0.0 || radius < 0.0) {
            throw new RuntimeException(String.valueOf("vol_spherical_cap() only accepts non-negative values"));
        }
        return (1.0 / 3.0) * PI * height * height * (3.0 * radius - height);
    }

    static double vol_sphere(double radius) {
        if (radius < 0.0) {
            throw new RuntimeException(String.valueOf("vol_sphere() only accepts non-negative values"));
        }
        return (4.0 / 3.0) * PI * radius * radius * radius;
    }

    static double vol_spheres_intersect(double radius_1, double radius_2, double centers_distance) {
        if (radius_1 < 0.0 || radius_2 < 0.0 || centers_distance < 0.0) {
            throw new RuntimeException(String.valueOf("vol_spheres_intersect() only accepts non-negative values"));
        }
        if (centers_distance == 0.0) {
            return vol_sphere(minf(radius_1, radius_2));
        }
        double h1 = (radius_1 - radius_2 + centers_distance) * (radius_1 + radius_2 - centers_distance) / (2.0 * centers_distance);
        double h2 = (radius_2 - radius_1 + centers_distance) * (radius_2 + radius_1 - centers_distance) / (2.0 * centers_distance);
        return vol_spherical_cap(h1, radius_2) + vol_spherical_cap(h2, radius_1);
    }

    static double vol_spheres_union(double radius_1, double radius_2, double centers_distance) {
        if (radius_1 <= 0.0 || radius_2 <= 0.0 || centers_distance < 0.0) {
            throw new RuntimeException(String.valueOf("vol_spheres_union() only accepts non-negative values, non-zero radius"));
        }
        if (centers_distance == 0.0) {
            return vol_sphere(maxf(radius_1, radius_2));
        }
        return vol_sphere(radius_1) + vol_sphere(radius_2) - vol_spheres_intersect(radius_1, radius_2, centers_distance);
    }

    static double vol_cuboid(double width, double height, double length) {
        if (width < 0.0 || height < 0.0 || length < 0.0) {
            throw new RuntimeException(String.valueOf("vol_cuboid() only accepts non-negative values"));
        }
        return width * height * length;
    }

    static double vol_cone(double area_of_base, double height) {
        if (height < 0.0 || area_of_base < 0.0) {
            throw new RuntimeException(String.valueOf("vol_cone() only accepts non-negative values"));
        }
        return area_of_base * height / 3.0;
    }

    static double vol_right_circ_cone(double radius, double height) {
        if (height < 0.0 || radius < 0.0) {
            throw new RuntimeException(String.valueOf("vol_right_circ_cone() only accepts non-negative values"));
        }
        return PI * radius * radius * height / 3.0;
    }

    static double vol_prism(double area_of_base, double height) {
        if (height < 0.0 || area_of_base < 0.0) {
            throw new RuntimeException(String.valueOf("vol_prism() only accepts non-negative values"));
        }
        return area_of_base * height;
    }

    static double vol_pyramid(double area_of_base, double height) {
        if (height < 0.0 || area_of_base < 0.0) {
            throw new RuntimeException(String.valueOf("vol_pyramid() only accepts non-negative values"));
        }
        return area_of_base * height / 3.0;
    }

    static double vol_hemisphere(double radius) {
        if (radius < 0.0) {
            throw new RuntimeException(String.valueOf("vol_hemisphere() only accepts non-negative values"));
        }
        return radius * radius * radius * PI * 2.0 / 3.0;
    }

    static double vol_circular_cylinder(double radius, double height) {
        if (height < 0.0 || radius < 0.0) {
            throw new RuntimeException(String.valueOf("vol_circular_cylinder() only accepts non-negative values"));
        }
        return radius * radius * height * PI;
    }

    static double vol_hollow_circular_cylinder(double inner_radius, double outer_radius, double height) {
        if (inner_radius < 0.0 || outer_radius < 0.0 || height < 0.0) {
            throw new RuntimeException(String.valueOf("vol_hollow_circular_cylinder() only accepts non-negative values"));
        }
        if (outer_radius <= inner_radius) {
            throw new RuntimeException(String.valueOf("outer_radius must be greater than inner_radius"));
        }
        return PI * (outer_radius * outer_radius - inner_radius * inner_radius) * height;
    }

    static double vol_conical_frustum(double height, double radius_1, double radius_2) {
        if (radius_1 < 0.0 || radius_2 < 0.0 || height < 0.0) {
            throw new RuntimeException(String.valueOf("vol_conical_frustum() only accepts non-negative values"));
        }
        return (1.0 / 3.0) * PI * height * (radius_1 * radius_1 + radius_2 * radius_2 + radius_1 * radius_2);
    }

    static double vol_torus(double torus_radius, double tube_radius) {
        if (torus_radius < 0.0 || tube_radius < 0.0) {
            throw new RuntimeException(String.valueOf("vol_torus() only accepts non-negative values"));
        }
        return 2.0 * PI * PI * torus_radius * tube_radius * tube_radius;
    }

    static double vol_icosahedron(double tri_side) {
        if (tri_side < 0.0) {
            throw new RuntimeException(String.valueOf("vol_icosahedron() only accepts non-negative values"));
        }
        return tri_side * tri_side * tri_side * (3.0 + SQRT5) * 5.0 / 12.0;
    }

    static void main() {
        System.out.println("Volumes:");
        System.out.println("Cube: " + _p(vol_cube(2.0)));
        System.out.println("Cuboid: " + _p(vol_cuboid(2.0, 2.0, 2.0)));
        System.out.println("Cone: " + _p(vol_cone(2.0, 2.0)));
        System.out.println("Right Circular Cone: " + _p(vol_right_circ_cone(2.0, 2.0)));
        System.out.println("Prism: " + _p(vol_prism(2.0, 2.0)));
        System.out.println("Pyramid: " + _p(vol_pyramid(2.0, 2.0)));
        System.out.println("Sphere: " + _p(vol_sphere(2.0)));
        System.out.println("Hemisphere: " + _p(vol_hemisphere(2.0)));
        System.out.println("Circular Cylinder: " + _p(vol_circular_cylinder(2.0, 2.0)));
        System.out.println("Torus: " + _p(vol_torus(2.0, 2.0)));
        System.out.println("Conical Frustum: " + _p(vol_conical_frustum(2.0, 2.0, 4.0)));
        System.out.println("Spherical cap: " + _p(vol_spherical_cap(1.0, 2.0)));
        System.out.println("Spheres intersection: " + _p(vol_spheres_intersect(2.0, 2.0, 1.0)));
        System.out.println("Spheres union: " + _p(vol_spheres_union(2.0, 2.0, 1.0)));
        System.out.println("Hollow Circular Cylinder: " + _p(vol_hollow_circular_cylinder(1.0, 2.0, 3.0)));
        System.out.println("Icosahedron: " + _p(vol_icosahedron(2.5)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            SQRT5 = 2.23606797749979;
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
        return String.valueOf(v);
    }
}
