public class Main {

    static double ndvi(double red, double nir) {
        return (nir - red) / (nir + red);
    }

    static double bndvi(double blue, double nir) {
        return (nir - blue) / (nir + blue);
    }

    static double gndvi(double green, double nir) {
        return (nir - green) / (nir + green);
    }

    static double ndre(double redEdge, double nir) {
        return (nir - redEdge) / (nir + redEdge);
    }

    static double ccci(double red, double redEdge, double nir) {
        return ndre(redEdge, nir) / ndvi(red, nir);
    }

    static double cvi(double red, double green, double nir) {
        return (nir * red) / (green * green);
    }

    static double gli(double red, double green, double blue) {
        return (2.0 * green - red - blue) / (2.0 * green + red + blue);
    }

    static double dvi(double red, double nir) {
        return nir / red;
    }

    static double calc(String index, double red, double green, double blue, double redEdge, double nir) {
        if ((index.equals("NDVI"))) {
            return ndvi(red, nir);
        }
        if ((index.equals("BNDVI"))) {
            return bndvi(blue, nir);
        }
        if ((index.equals("GNDVI"))) {
            return gndvi(green, nir);
        }
        if ((index.equals("NDRE"))) {
            return ndre(redEdge, nir);
        }
        if ((index.equals("CCCI"))) {
            return ccci(red, redEdge, nir);
        }
        if ((index.equals("CVI"))) {
            return cvi(red, green, nir);
        }
        if ((index.equals("GLI"))) {
            return gli(red, green, blue);
        }
        if ((index.equals("DVI"))) {
            return dvi(red, nir);
        }
        return 0.0;
    }

    static void main() {
        double red = 50.0;
        double green = 30.0;
        double blue = 10.0;
        double redEdge = 40.0;
        double nir = 100.0;
        System.out.println("NDVI=" + _p(ndvi(red, nir)));
        System.out.println("CCCI=" + _p(ccci(red, redEdge, nir)));
        System.out.println("CVI=" + _p(cvi(red, green, nir)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
