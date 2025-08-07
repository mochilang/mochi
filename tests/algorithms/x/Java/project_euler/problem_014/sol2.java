public class Main {
    static java.util.Map<Integer,Integer> collatz_cache = null;
    static String input_str;
    static int limit;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int collatz_length(int n) {
        int num = n;
        int[] sequence = ((int[])(new int[]{}));
        while (!(Boolean)(collatz_cache.containsKey(num))) {
            sequence = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sequence), java.util.stream.IntStream.of(num)).toArray()));
            if (Math.floorMod(num, 2) == 0) {
                num = ((Number)((Math.floorDiv(num, 2)))).intValue();
            } else {
                num = 3 * num + 1;
            }
        }
        int length = (int)(((int)(collatz_cache).getOrDefault(num, 0)));
        int i = sequence.length - 1;
        while (i >= 0) {
            length = length + 1;
collatz_cache.put(sequence[i], length);
            i = i - 1;
        }
        return length;
    }

    static int solution(int limit) {
        int max_len = 0;
        int max_start = 1;
        int i_1 = 1;
        while (i_1 < limit) {
            int length_1 = collatz_length(i_1);
            if (length_1 > max_len) {
                max_len = length_1;
                max_start = i_1;
            }
            i_1 = i_1 + 1;
        }
        return max_start;
    }
    public static void main(String[] args) {
        collatz_cache = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>(java.util.Map.ofEntries(java.util.Map.entry(1, 1)))));
        input_str = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        limit = Integer.parseInt(input_str);
        System.out.println(solution(limit));
    }
}
