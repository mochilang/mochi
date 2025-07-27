public class Main {

    static int countOccurrences(String s, String sub) {
        if (sub.length() == 0) {
            return s.length() + 1;
        }
        int cnt = 0;
        int i = 0;
        int step = sub.length();
        while (i + step <= s.length()) {
            if ((s.substring(i, i + step).equals(sub))) {
                cnt = cnt + 1;
                i = i + step;
            } else {
                i = i + 1;
            }
        }
        return cnt;
    }

    static void main() {
        System.out.println(String.valueOf(countOccurrences("the three truths", "th")));
        System.out.println(String.valueOf(countOccurrences("ababababab", "abab")));
    }
    public static void main(String[] args) {
        main();
    }
}
