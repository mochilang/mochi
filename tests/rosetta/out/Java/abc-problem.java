// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// abc-problem.mochi
import java.util.*;

public class AbcProblem {
    static List<String> fields(String s) {
        List<String> res = new ArrayList<>(Arrays.asList());
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String c = s.substring(i, i + 1);
            if (Objects.equals(c, " ")) {
                if (cur.length() > 0) {
                    res.add(cur);
                    cur = "";
                }
            }
            else {
                cur = cur + c;
            }
            i = (int)(i + 1);
        }
        if (cur.length() > 0) {
            res.add(cur);
        }
        return res;
    }
    static boolean canSpell(String word, List<String> blks) {
        if (Objects.equals(word.length(), 0)) {
            return true;
        }
        String c = String.valueOf(word.substring(0, 1)).toLowerCase();
        int i = 0;
        while (i < blks.size()) {
            List<String> b = blks.get(i);
            if (Objects.equals(c, String.valueOf(((List)b).subList(0, 1)).toLowerCase()) || Objects.equals(c, String.valueOf(((List)b).subList(1, 2)).toLowerCase())) {
                List<String> rest = new ArrayList<>(Arrays.asList());
                int j = 0;
                while (j < blks.size()) {
                    if (j != i) {
                        rest.add(blks.get(j));
                    }
                    j = (int)(j + 1);
                }
                if (canSpell(word.substring(1, word.length()), rest)) {
                    return true;
                }
            }
            i = (int)(i + 1);
        }
        return false;
    }
    static Object newSpeller(String blocks) {
        List<String> bl = fields(blocks);
        return w -> canSpell(w, bl);
    }
    static void main() {
        Object sp = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM");
        for (String word : new ArrayList<>(Arrays.asList("A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"))) {
            System.out.println(word + " " + String.valueOf(sp(word)));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
