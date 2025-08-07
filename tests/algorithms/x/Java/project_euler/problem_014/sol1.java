public class Main {
    static String input_str;
    static int n;

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int solution(int n) {
        int[] counters = new int[0];
        int i = 0;
        while (i <= n) {
            counters = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counters), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
counters[1] = 1;
        int largest_number = 1;
        int pre_counter = 1;
        int start = 2;
        while (start < n) {
            int number = start;
            int counter = 0;
            while (true) {
                if (number < counters.length && counters[number] != 0) {
                    counter = counter + counters[number];
                    break;
                }
                if (Math.floorMod(number, 2) == 0) {
                    number = Math.floorDiv(number, 2);
                } else {
                    number = 3 * number + 1;
                }
                counter = counter + 1;
            }
            if (start < counters.length && counters[start] == 0) {
counters[start] = counter;
            }
            if (counter > pre_counter) {
                largest_number = start;
                pre_counter = counter;
            }
            start = start + 1;
        }
        return largest_number;
    }
    public static void main(String[] args) {
        input_str = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        n = Integer.parseInt(input_str);
        System.out.println(_p(solution(n)));
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
