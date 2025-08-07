public class Main {
    static int result;

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        return x;
    }

    static int solution(int limit) {
        java.util.Map<Integer,Integer> frequencies = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        int m = 2;
        while (2 * m * (m + 1) <= limit) {
            int n = (Math.floorMod(m, 2)) + 1;
            while (n < m) {
                if (gcd(m, n) > 1) {
                    n = n + 2;
                    continue;
                }
                int primitive_perimeter = 2 * m * (m + n);
                int perimeter = primitive_perimeter;
                while (perimeter <= limit) {
                    if (!(Boolean)(frequencies.containsKey(perimeter))) {
frequencies.put(perimeter, 0);
                    }
frequencies.put(perimeter, (int)(((int)(frequencies).getOrDefault(perimeter, 0))) + 1);
                    perimeter = perimeter + primitive_perimeter;
                }
                n = n + 2;
            }
            m = m + 1;
        }
        int count = 0;
        for (int p : frequencies.keySet()) {
            if ((int)(((int)(frequencies).getOrDefault(p, 0))) == 1) {
                count = count + 1;
            }
        }
        return count;
    }
    public static void main(String[] args) {
        result = solution(1500000);
        System.out.println("solution() = " + _p(result));
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
