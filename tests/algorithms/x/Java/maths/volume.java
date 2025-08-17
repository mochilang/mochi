public class Main {
    static double PI = (double)(3.141592653589793);
    static double SQRT5 = (double)(2.23606797749979);

    static double minf(double a, double b) {
        if ((double)(a) < (double)(b)) {
            return a;
        }
        return b;
    }

    static double maxf(double a, double b) {
        if ((double)(a) > (double)(b)) {
            return a;
        }
        return b;
    }

    static double vol_cube(double side_length) {
        if ((double)(side_length) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_cube() only accepts non-negative values"));
        }
        return (double)((double)(side_length) * (double)(side_length)) * (double)(side_length);
    }

    static double vol_spherical_cap(double height, double radius) {
        if ((double)(height) < (double)(0.0) || (double)(radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_spherical_cap() only accepts non-negative values"));
        }
        return (double)((double)((double)((double)(((double)(1.0) / (double)(3.0))) * (double)(PI)) * (double)(height)) * (double)(height)) * (double)(((double)((double)(3.0) * (double)(radius)) - (double)(height)));
    }

    static double vol_sphere(double radius) {
        if ((double)(radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_sphere() only accepts non-negative values"));
        }
        return (double)((double)((double)((double)(((double)(4.0) / (double)(3.0))) * (double)(PI)) * (double)(radius)) * (double)(radius)) * (double)(radius);
    }

    static double vol_spheres_intersect(double radius_1, double radius_2, double centers_distance) {
        if ((double)(radius_1) < (double)(0.0) || (double)(radius_2) < (double)(0.0) || (double)(centers_distance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_spheres_intersect() only accepts non-negative values"));
        }
        if ((double)(centers_distance) == (double)(0.0)) {
            return vol_sphere((double)(minf((double)(radius_1), (double)(radius_2))));
        }
        double h1_1 = (double)((double)((double)(((double)((double)(radius_1) - (double)(radius_2)) + (double)(centers_distance))) * (double)(((double)((double)(radius_1) + (double)(radius_2)) - (double)(centers_distance)))) / (double)(((double)(2.0) * (double)(centers_distance))));
        double h2_1 = (double)((double)((double)(((double)((double)(radius_2) - (double)(radius_1)) + (double)(centers_distance))) * (double)(((double)((double)(radius_2) + (double)(radius_1)) - (double)(centers_distance)))) / (double)(((double)(2.0) * (double)(centers_distance))));
        return (double)(vol_spherical_cap((double)(h1_1), (double)(radius_2))) + (double)(vol_spherical_cap((double)(h2_1), (double)(radius_1)));
    }

    static double vol_spheres_union(double radius_1, double radius_2, double centers_distance) {
        if ((double)(radius_1) <= (double)(0.0) || (double)(radius_2) <= (double)(0.0) || (double)(centers_distance) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_spheres_union() only accepts non-negative values, non-zero radius"));
        }
        if ((double)(centers_distance) == (double)(0.0)) {
            return vol_sphere((double)(maxf((double)(radius_1), (double)(radius_2))));
        }
        return (double)((double)(vol_sphere((double)(radius_1))) + (double)(vol_sphere((double)(radius_2)))) - (double)(vol_spheres_intersect((double)(radius_1), (double)(radius_2), (double)(centers_distance)));
    }

    static double vol_cuboid(double width, double height, double length) {
        if ((double)(width) < (double)(0.0) || (double)(height) < (double)(0.0) || (double)(length) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_cuboid() only accepts non-negative values"));
        }
        return (double)((double)(width) * (double)(height)) * (double)(length);
    }

    static double vol_cone(double area_of_base, double height) {
        if ((double)(height) < (double)(0.0) || (double)(area_of_base) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_cone() only accepts non-negative values"));
        }
        return (double)((double)(area_of_base) * (double)(height)) / (double)(3.0);
    }

    static double vol_right_circ_cone(double radius, double height) {
        if ((double)(height) < (double)(0.0) || (double)(radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_right_circ_cone() only accepts non-negative values"));
        }
        return (double)((double)((double)((double)(PI) * (double)(radius)) * (double)(radius)) * (double)(height)) / (double)(3.0);
    }

    static double vol_prism(double area_of_base, double height) {
        if ((double)(height) < (double)(0.0) || (double)(area_of_base) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_prism() only accepts non-negative values"));
        }
        return (double)(area_of_base) * (double)(height);
    }

    static double vol_pyramid(double area_of_base, double height) {
        if ((double)(height) < (double)(0.0) || (double)(area_of_base) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_pyramid() only accepts non-negative values"));
        }
        return (double)((double)(area_of_base) * (double)(height)) / (double)(3.0);
    }

    static double vol_hemisphere(double radius) {
        if ((double)(radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_hemisphere() only accepts non-negative values"));
        }
        return (double)((double)((double)((double)((double)(radius) * (double)(radius)) * (double)(radius)) * (double)(PI)) * (double)(2.0)) / (double)(3.0);
    }

    static double vol_circular_cylinder(double radius, double height) {
        if ((double)(height) < (double)(0.0) || (double)(radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_circular_cylinder() only accepts non-negative values"));
        }
        return (double)((double)((double)(radius) * (double)(radius)) * (double)(height)) * (double)(PI);
    }

    static double vol_hollow_circular_cylinder(double inner_radius, double outer_radius, double height) {
        if ((double)(inner_radius) < (double)(0.0) || (double)(outer_radius) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_hollow_circular_cylinder() only accepts non-negative values"));
        }
        if ((double)(outer_radius) <= (double)(inner_radius)) {
            throw new RuntimeException(String.valueOf("outer_radius must be greater than inner_radius"));
        }
        return (double)((double)(PI) * (double)(((double)((double)(outer_radius) * (double)(outer_radius)) - (double)((double)(inner_radius) * (double)(inner_radius))))) * (double)(height);
    }

    static double vol_conical_frustum(double height, double radius_1, double radius_2) {
        if ((double)(radius_1) < (double)(0.0) || (double)(radius_2) < (double)(0.0) || (double)(height) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_conical_frustum() only accepts non-negative values"));
        }
        return (double)((double)((double)(((double)(1.0) / (double)(3.0))) * (double)(PI)) * (double)(height)) * (double)(((double)((double)((double)(radius_1) * (double)(radius_1)) + (double)((double)(radius_2) * (double)(radius_2))) + (double)((double)(radius_1) * (double)(radius_2))));
    }

    static double vol_torus(double torus_radius, double tube_radius) {
        if ((double)(torus_radius) < (double)(0.0) || (double)(tube_radius) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_torus() only accepts non-negative values"));
        }
        return (double)((double)((double)((double)((double)(2.0) * (double)(PI)) * (double)(PI)) * (double)(torus_radius)) * (double)(tube_radius)) * (double)(tube_radius);
    }

    static double vol_icosahedron(double tri_side) {
        if ((double)(tri_side) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("vol_icosahedron() only accepts non-negative values"));
        }
        return (double)((double)((double)((double)((double)(tri_side) * (double)(tri_side)) * (double)(tri_side)) * (double)(((double)(3.0) + (double)(SQRT5)))) * (double)(5.0)) / (double)(12.0);
    }

    static void main() {
        System.out.println("Volumes:");
        System.out.println("Cube: " + _p(vol_cube((double)(2.0))));
        System.out.println("Cuboid: " + _p(vol_cuboid((double)(2.0), (double)(2.0), (double)(2.0))));
        System.out.println("Cone: " + _p(vol_cone((double)(2.0), (double)(2.0))));
        System.out.println("Right Circular Cone: " + _p(vol_right_circ_cone((double)(2.0), (double)(2.0))));
        System.out.println("Prism: " + _p(vol_prism((double)(2.0), (double)(2.0))));
        System.out.println("Pyramid: " + _p(vol_pyramid((double)(2.0), (double)(2.0))));
        System.out.println("Sphere: " + _p(vol_sphere((double)(2.0))));
        System.out.println("Hemisphere: " + _p(vol_hemisphere((double)(2.0))));
        System.out.println("Circular Cylinder: " + _p(vol_circular_cylinder((double)(2.0), (double)(2.0))));
        System.out.println("Torus: " + _p(vol_torus((double)(2.0), (double)(2.0))));
        System.out.println("Conical Frustum: " + _p(vol_conical_frustum((double)(2.0), (double)(2.0), (double)(4.0))));
        System.out.println("Spherical cap: " + _p(vol_spherical_cap((double)(1.0), (double)(2.0))));
        System.out.println("Spheres intersection: " + _p(vol_spheres_intersect((double)(2.0), (double)(2.0), (double)(1.0))));
        System.out.println("Spheres union: " + _p(vol_spheres_union((double)(2.0), (double)(2.0), (double)(1.0))));
        System.out.println("Hollow Circular Cylinder: " + _p(vol_hollow_circular_cylinder((double)(1.0), (double)(2.0), (double)(3.0))));
        System.out.println("Icosahedron: " + _p(vol_icosahedron((double)(2.5))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
