// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// bitmap-read-an-image-through-a-pipe.mochi
import java.util.*;

public class BitmapReadAnImageThroughAPipe {
    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if (str.length() > 0 && Objects.equals(str.substring(0, 1), "-")) {
            neg = true;
            i = (int)(1);
        }
        int n = 0;
        Digit digits = new Digit(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
        while (i < str.length()) {
            n = (int)(n * 10 + ((Number)((Map<?,?>)digits).get(str.substring(i, i + 1))).doubleValue());
            i = (int)(i + 1);
        }
        if (neg) {
            n = (int)(-n);
        }
        return n;
    }
    static List<String> splitWs(String s) {
        List<String> parts = Arrays.asList();
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if (Objects.equals(ch, " ") || Objects.equals(ch, "\n") || Objects.equals(ch, "\t") || Objects.equals(ch, "\r")) {
                if (cur.length() > 0) {
                    parts.add(cur);
                    cur = "";
                }
            }
            else {
                cur = cur + ch;
            }
            i = (int)(i + 1);
        }
        if (cur.length() > 0) {
            parts.add(cur);
        }
        return parts;
    }
    static Map<String,Object> parsePpm(String data) {
        List<String> toks = splitWs(data);
        if (toks.size() < 4) {
            return new LinkedHashMap<>(Map.ofEntries(Map.entry("err", true)));
        }
        List<String> magic = toks.get(0);
        int w = parseIntStr(toks.get(1));
        int h = parseIntStr(toks.get(2));
        int maxv = parseIntStr(toks.get(3));
        List<Integer> px = Arrays.asList();
        int i = 4;
        while (i < toks.size()) {
            px.add(parseIntStr(toks.get(i)));
            i = (int)(i + 1);
        }
        return new MagicWHMaxPx(magic, w, h, maxv, px);
    }
    public static void main(String[] args) {
        String ppmData = "P3\n2 2\n1\n0 1 1 0 1 0 0 1 1 1 0 0\n";
        Map<String,Object> img = parsePpm(ppmData);
        System.out.println(((Number)((Number)"width=" + ((Number)String.valueOf(((Map<String,Object>)img).get("w"))).doubleValue()).doubleValue() + " height=").doubleValue() + ((Number)String.valueOf(((Map<String,Object>)img).get("h"))).doubleValue());
    }
}
