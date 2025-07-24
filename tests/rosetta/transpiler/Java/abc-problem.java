public class Main {

    static String[] fields(String s) {
        String[] res = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String c = s.substring(i, i + 1);
            if ((c.equals(" "))) {
                if (cur.length() > 0) {
                    res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + c;
            }
            i = i + 1;
        }
        if (cur.length() > 0) {
            res = java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        return res;
    }

    static boolean canSpell(String word, String[] blks) {
        if (word.length() == 0) {
            return true;
        }
        String c = word.substring(0, 1).toLowerCase();
        int i = 0;
        while (i < blks.length) {
            String b = blks[i];
            if (((c.equals(b.substring(0, 1).toLowerCase())) || c.equals(b.substring(1, 2).toLowerCase()))) {
                String[] rest = new String[]{};
                int j = 0;
                while (j < blks.length) {
                    if (j != i) {
                        rest = java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(blks[j])).toArray(String[]::new);
                    }
                    j = j + 1;
                }
                if (canSpell(word.substring(1, word.length()), rest)) {
                    return true;
                }
            }
            i = i + 1;
        }
        return false;
    }

    static java.util.function.Function<String,Boolean> newSpeller(String blocks) {
        String[] bl = fields(blocks);
        return (w) -> canSpell(w, bl);
    }

    static void main() {
        java.util.function.Function<String,Boolean> sp = newSpeller("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM");
        for (var word : new String[]{"A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"}) {
            System.out.println(word + " " + String.valueOf(sp.apply(word)));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
