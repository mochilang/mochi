public class Main {
    static String[] strings = new String[]{"", "\"If I were two-faced, would I be wearing this one?\" --- Abraham Lincoln ", "..1111111111111111111111111111111111111111111111111111111111111117777888", "I never give 'em hell, I just tell the truth, and they think it's hell. ", "                                                   ---  Harry S Truman  ", "The better the 4-wheel drive, the further you'll be from help when ya get stuck!", "headmistressship", "aardvark", "😍😀🙌💃😍😍😍🙌"};
    static String[][] chars = new String[][]{new String[]{" "}, new String[]{"-"}, new String[]{"7"}, new String[]{"."}, new String[]{" ", "-", "r"}, new String[]{"e"}, new String[]{"s"}, new String[]{"a"}, new String[]{"😍"}};
    static int i = 0;

    static String padLeft(int n, int width) {
        String s = String.valueOf(n);
        while (_runeLen(s) < width) {
            s = " " + s;
        }
        return s;
    }

    static String squeeze(String s, String ch) {
        String out = "";
        boolean prev = false;
        int i = 0;
        while (i < _runeLen(s)) {
            String c = _substr(s, i, i + 1);
            if ((c.equals(ch))) {
                if (!prev) {
                    out = out + c;
                    prev = true;
                }
            } else {
                out = out + c;
                prev = false;
            }
            i = i + 1;
        }
        return out;
    }
    public static void main(String[] args) {
        while (i < strings.length) {
            int j = 0;
            String s = strings[i];
            while (j < chars[i].length) {
                String c = chars[i][j];
                String ss = String.valueOf(squeeze(s, c));
                System.out.println("specified character = '" + c + "'");
                System.out.println("original : length = " + String.valueOf(padLeft(_runeLen(s), 2)) + ", string = «««" + s + "»»»");
                System.out.println("squeezed : length = " + String.valueOf(padLeft(_runeLen(ss), 2)) + ", string = «««" + ss + "»»»");
                System.out.println("");
                j = j + 1;
            }
            i = i + 1;
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
