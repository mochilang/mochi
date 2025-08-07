public class Main {
    static int[] DIGIT_FACTORIALS;
    static java.util.Map<Integer,Integer> cache_sum_digit_factorials = null;
    static java.util.Map<Integer,Integer> chain_length_cache = null;

    static int sum_digit_factorials(int n) {
        if (((Boolean)(cache_sum_digit_factorials.containsKey(n)))) {
            return ((int)(cache_sum_digit_factorials).getOrDefault(n, 0));
        }
        int m = n;
        int ret = 0;
        if (m == 0) {
            ret = DIGIT_FACTORIALS[0];
        }
        while (m > 0) {
            int digit = Math.floorMod(m, 10);
            ret = ret + DIGIT_FACTORIALS[digit];
            m = Math.floorDiv(m, 10);
        }
cache_sum_digit_factorials.put(n, ret);
        return ret;
    }

    static int chain_length(int n) {
        if (((Boolean)(chain_length_cache.containsKey(n)))) {
            return ((int)(chain_length_cache).getOrDefault(n, 0));
        }
        int[] chain = ((int[])(new int[]{}));
        java.util.Map<Integer,Integer> seen = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        int current = n;
        while (true) {
            if (((Boolean)(chain_length_cache.containsKey(current)))) {
                int known = (int)(((int)(chain_length_cache).getOrDefault(current, 0)));
                int total = known;
                int i = chain.length - 1;
                while (i >= 0) {
                    total = total + 1;
chain_length_cache.put(chain[i], total);
                    i = i - 1;
                }
                return ((int)(chain_length_cache).getOrDefault(n, 0));
            }
            if (((Boolean)(seen.containsKey(current)))) {
                int loop_start = (int)(((int)(seen).getOrDefault(current, 0)));
                int loop_len = chain.length - loop_start;
                int i_1 = chain.length - 1;
                int ahead = 0;
                while (i_1 >= 0) {
                    if (i_1 >= loop_start) {
chain_length_cache.put(chain[i_1], loop_len);
                    } else {
chain_length_cache.put(chain[i_1], loop_len + (ahead + 1));
                    }
                    ahead = ahead + 1;
                    i_1 = i_1 - 1;
                }
                return ((int)(chain_length_cache).getOrDefault(n, 0));
            }
seen.put(current, chain.length);
            chain = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(chain), java.util.stream.IntStream.of(current)).toArray()));
            current = sum_digit_factorials(current);
        }
    }

    static int solution(int num_terms, int max_start) {
        int count = 0;
        int i_2 = 1;
        while (i_2 < max_start) {
            if (chain_length(i_2) == num_terms) {
                count = count + 1;
            }
            i_2 = i_2 + 1;
        }
        return count;
    }
    public static void main(String[] args) {
        DIGIT_FACTORIALS = ((int[])(new int[]{1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880}));
        cache_sum_digit_factorials = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>(java.util.Map.ofEntries(java.util.Map.entry(145, 145)))));
        chain_length_cache = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>(java.util.Map.ofEntries(java.util.Map.entry(145, 0), java.util.Map.entry(169, 3), java.util.Map.entry(36301, 3), java.util.Map.entry(1454, 3), java.util.Map.entry(871, 2), java.util.Map.entry(45361, 2), java.util.Map.entry(872, 2)))));
        System.out.println("solution() = " + _p(solution(60, 1000)));
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
