public class Main {
    static class FuzzySet {
        String name;
        double left_boundary;
        double peak;
        double right_boundary;
        FuzzySet(String name, double left_boundary, double peak, double right_boundary) {
            this.name = name;
            this.left_boundary = left_boundary;
            this.peak = peak;
            this.right_boundary = right_boundary;
        }
        FuzzySet() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'left_boundary': %s, 'peak': %s, 'right_boundary': %s}", String.valueOf(name), String.valueOf(left_boundary), String.valueOf(peak), String.valueOf(right_boundary));
        }
    }

    static FuzzySet sheru;
    static FuzzySet siya;
    static FuzzySet sheru_comp;
    static FuzzySet inter;
    static FuzzySet uni;

    static String stringify(FuzzySet fs) {
        return fs.name + ": [" + _p(fs.left_boundary) + ", " + _p(fs.peak) + ", " + _p(fs.right_boundary) + "]";
    }

    static double max2(double a, double b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static double min2(double a, double b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static FuzzySet complement(FuzzySet fs) {
        return new FuzzySet("¬" + fs.name, 1.0 - fs.right_boundary, 1.0 - fs.left_boundary, 1.0 - fs.peak);
    }

    static FuzzySet intersection(FuzzySet a, FuzzySet b) {
        return new FuzzySet(a.name + " ∩ " + b.name, max2(a.left_boundary, b.left_boundary), min2(a.right_boundary, b.right_boundary), (a.peak + b.peak) / 2.0);
    }

    static FuzzySet union(FuzzySet a, FuzzySet b) {
        return new FuzzySet(a.name + " U " + b.name, min2(a.left_boundary, b.left_boundary), max2(a.right_boundary, b.right_boundary), (a.peak + b.peak) / 2.0);
    }

    static double membership(FuzzySet fs, double x) {
        if (x <= fs.left_boundary || x >= fs.right_boundary) {
            return 0.0;
        }
        if (fs.left_boundary < x && x <= fs.peak) {
            return (x - fs.left_boundary) / (fs.peak - fs.left_boundary);
        }
        if (fs.peak < x && x < fs.right_boundary) {
            return (fs.right_boundary - x) / (fs.right_boundary - fs.peak);
        }
        return 0.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sheru = new FuzzySet("Sheru", 0.4, 1.0, 0.6);
            siya = new FuzzySet("Siya", 0.5, 1.0, 0.7);
            System.out.println(stringify(sheru));
            System.out.println(stringify(siya));
            sheru_comp = complement(sheru);
            System.out.println(stringify(sheru_comp));
            inter = intersection(siya, sheru);
            System.out.println(stringify(inter));
            System.out.println("Sheru membership 0.5: " + _p(membership(sheru, 0.5)));
            System.out.println("Sheru membership 0.6: " + _p(membership(sheru, 0.6)));
            uni = union(siya, sheru);
            System.out.println(stringify(uni));
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
