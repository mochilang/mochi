public class Main {

    static int int_sqrt(int n) {
        int low = 0;
        int high = n;
        while (low <= high) {
            int mid = Math.floorDiv((low + high), 2);
            int sq = mid * mid;
            if (sq == n) {
                return mid;
            }
            if (sq < n) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        return high;
    }

    static java.util.Map<Integer,Integer> pythagorean_triple(int max_perimeter) {
        java.util.Map<Integer,Integer> triplets = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        int base = 1;
        while (base <= max_perimeter) {
            int perpendicular = base;
            while (perpendicular <= max_perimeter) {
                int hyp_sq = base * base + perpendicular * perpendicular;
                int hyp = int_sqrt(hyp_sq);
                if (hyp * hyp == hyp_sq) {
                    int perimeter = base + perpendicular + hyp;
                    if (perimeter <= max_perimeter) {
                        if (((Boolean)(triplets.containsKey(perimeter)))) {
triplets.put(perimeter, (int)(((int)(triplets).getOrDefault(perimeter, 0))) + 1);
                        } else {
triplets.put(perimeter, 1);
                        }
                    }
                }
                perpendicular = perpendicular + 1;
            }
            base = base + 1;
        }
        return triplets;
    }

    static int max_perimeter(java.util.Map<Integer,Integer> counts) {
        int best_p = 0;
        int max_count = 0;
        for (int p : counts.keySet()) {
            int count = (int)(((int)(counts).getOrDefault(p, 0)));
            if ((count > max_count) || ((count == max_count) && (p > best_p))) {
                max_count = count;
                best_p = p;
            }
        }
        return best_p;
    }

    static int solution(int n) {
        java.util.Map<Integer,Integer> triplets_1 = pythagorean_triple(n);
        return max_perimeter(triplets_1);
    }

    static void main() {
        int s200 = max_perimeter(pythagorean_triple(200));
        int s100 = max_perimeter(pythagorean_triple(100));
        System.out.println(_p(s100));
        System.out.println(_p(s200));
        int best = max_perimeter(pythagorean_triple(1000));
        System.out.println(_p(best));
        System.out.println("Perimeter " + _p(best) + " has maximum solutions");
    }
    public static void main(String[] args) {
        main();
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
