public class Main {
    static String N;

    static int str_eval(String s) {
        int product = 1;
        int i = 0;
        while (i < _runeLen(s)) {
            product = product * (Integer.parseInt(s.substring(i, i + 1)));
            i = i + 1;
        }
        return product;
    }

    static int solution(String n) {
        int largest_product = -1;
        String substr = n.substring(0, 13);
        int cur_index = 13;
        while (cur_index < _runeLen(n) - 13) {
            if ((Integer.parseInt(n.substring(cur_index, cur_index + 1))) >= (Integer.parseInt(substr.substring(0, 1)))) {
                substr = substr.substring(1, _runeLen(substr)) + n.substring(cur_index, cur_index + 1);
                cur_index = cur_index + 1;
            } else {
                int prod = str_eval(substr);
                if (prod > largest_product) {
                    largest_product = prod;
                }
                substr = n.substring(cur_index, cur_index + 13);
                cur_index = cur_index + 13;
            }
        }
        return largest_product;
    }

    static void main() {
        int res = solution(N);
        System.out.println("solution() = " + _p(res));
    }
    public static void main(String[] args) {
        N = "73167176531330624919225119674426574742355349194934" + "96983520312774506326239578318016984801869478851843" + "85861560789112949495459501737958331952853208805511" + "12540698747158523863050715693290963295227443043557" + "66896648950445244523161731856403098711121722383113" + "62229893423380308135336276614282806444486645238749" + "30358907296290491560440772390713810515859307960866" + "70172427121883998797908792274921901699720888093776" + "65727333001053367881220235421809751254540594752243" + "52584907711670556013604839586446706324415722155397" + "53697817977846174064955149290862569321978468622482" + "83972241375657056057490261407972968652414535100474" + "82166370484403199890008895243450658541227588666881" + "16427171479924442928230863465674813919123162824586" + "17866458359124566529476545682848912883142607690042" + "24219022671055626321111109370544217506941658960408" + "07198403850962455444362981230987879927244284909188" + "84580156166097919133875499200524063689912560717606" + "05886116467109405077541002256983155200055935729725" + "71636269561882670428252483600823257530420752963450";
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
