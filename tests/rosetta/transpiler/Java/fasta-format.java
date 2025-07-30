public class Main {
    static String FASTA = ">Rosetta_Example_1\n" + "THERECANBENOSPACE\n" + ">Rosetta_Example_2\n" + "THERECANBESEVERAL\n" + "LINESBUTTHEYALLMUST\n" + "BECONCATENATED";

    static String[] splitLines(String s) {
        String[] lines = new String[]{};
        int start = 0;
        int i = 0;
        while (i < _runeLen(s)) {
            if ((_substr(s, i, i + 1).equals("\n"))) {
                lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(_substr(s, start, i))).toArray(String[]::new);
                i = i + 1;
                start = i;
            } else {
                i = i + 1;
            }
        }
        lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(_substr(s, start, _runeLen(s)))).toArray(String[]::new);
        return lines;
    }

    static String[] parseFasta(String text) {
        String key = "";
        String val = "";
        String[] out = new String[]{};
        for (String line : splitLines(text)) {
            if ((line.equals(""))) {
                continue;
            }
            if ((_substr(line, 0, 1).equals(">"))) {
                if (!(key.equals(""))) {
                    out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(key + ": " + val)).toArray(String[]::new);
                }
                String hdr = _substr(line, 1, _runeLen(line));
                int idx = 0;
                while (idx < _runeLen(hdr) && !(_substr(hdr, idx, idx + 1).equals(" "))) {
                    idx = idx + 1;
                }
                key = _substr(hdr, 0, idx);
                val = "";
            } else {
                if ((key.equals(""))) {
                    System.out.println("missing header");
                    return new String[]{};
                }
                val = val + line;
            }
        }
        if (!(key.equals(""))) {
            out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(key + ": " + val)).toArray(String[]::new);
        }
        return out;
    }

    static void main() {
        String[] res = parseFasta(FASTA);
        for (String line : res) {
            System.out.println(line);
        }
    }
    public static void main(String[] args) {
        main();
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
