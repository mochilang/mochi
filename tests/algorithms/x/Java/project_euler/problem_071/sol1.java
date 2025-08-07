public class Main {

    static int solution(int numerator, int denominator, int limit) {
        int maxNumerator = 0;
        int maxDenominator = 1;
        int currentDenominator = 1;
        while (currentDenominator <= limit) {
            int currentNumerator = Math.floorDiv(currentDenominator * numerator, denominator);
            if (Math.floorMod(currentDenominator, denominator) == 0) {
                currentNumerator = currentNumerator - 1;
            }
            if (currentNumerator * maxDenominator > currentDenominator * maxNumerator) {
                maxNumerator = currentNumerator;
                maxDenominator = currentDenominator;
            }
            currentDenominator = currentDenominator + 1;
        }
        return maxNumerator;
    }
    public static void main(String[] args) {
        System.out.println(_p(solution(3, 7, 1000000)));
        System.out.println(_p(solution(3, 7, 8)));
        System.out.println(_p(solution(6, 7, 60)));
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
