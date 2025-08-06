public class Main {

    static boolean perfect_cube(int n) {
        int m = n;
        if (m < 0) {
            m = -m;
        }
        int i = 0;
        while (i * i * i < m) {
            i = i + 1;
        }
        return i * i * i == m;
    }

    static boolean perfect_cube_binary_search(int n) {
        int m_1 = n;
        if (m_1 < 0) {
            m_1 = -m_1;
        }
        int left = 0;
        int right = m_1;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            int cube = mid * mid * mid;
            if (cube == m_1) {
                return true;
            }
            if (cube < m_1) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return false;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(perfect_cube(27)));
            System.out.println(_p(perfect_cube(4)));
            System.out.println(_p(perfect_cube_binary_search(27)));
            System.out.println(_p(perfect_cube_binary_search(64)));
            System.out.println(_p(perfect_cube_binary_search(4)));
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
