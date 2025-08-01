public class Main {
    static String[] lines;
    static String[] blocks;
    static String[] outLines;

    static String repeat(String s, int n) {
        String out = "";
        int i = 0;
        while (i < n) {
            out = out + s;
            i = i + 1;
        }
        return out;
    }

    static String trimRightSpace(String s) {
        int i_1 = _runeLen(s) - 1;
        while (i_1 >= 0 && (s.substring(i_1, i_1 + 1).equals(" "))) {
            i_1 = i_1 - 1;
        }
        return s.substring(0, i_1 + 1);
    }

    static String[] block2text(String[] block) {
        String[] out_1 = new String[]{};
        for (String b : block) {
            out_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(out_1), java.util.stream.Stream.of(trimRightSpace(b))).toArray(String[]::new);
        }
        return out_1;
    }

    static String[] text2block(String[] lines) {
        String[] out_2 = new String[]{};
        int count = 0;
        for (String line : lines) {
            String s = line;
            int le = _runeLen(s);
            if (le > 64) {
                s = s.substring(0, 64);
            } else             if (le < 64) {
                s = s + String.valueOf(repeat(" ", 64 - le));
            }
            out_2 = java.util.stream.Stream.concat(java.util.Arrays.stream(out_2), java.util.stream.Stream.of(s)).toArray(String[]::new);
            count = count + 1;
        }
        if (Math.floorMod(count, 16) != 0) {
            int pad = 16 - Math.floorMod(count, 16);
            int i_2 = 0;
            while (i_2 < pad) {
                out_2 = java.util.stream.Stream.concat(java.util.Arrays.stream(out_2), java.util.stream.Stream.of(repeat(" ", 64))).toArray(String[]::new);
                i_2 = i_2 + 1;
            }
        }
        return out_2;
    }
    public static void main(String[] args) {
        lines = new String[]{"alpha", "beta", "gamma"};
        blocks = text2block(lines);
        outLines = block2text(blocks);
        for (String l : outLines) {
            if (!(l.equals(""))) {
                System.out.println(l);
            }
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
