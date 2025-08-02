public class Main {
    static String FASTA;

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
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            FASTA = ">Rosetta_Example_1\n" + "THERECANBENOSPACE\n" + ">Rosetta_Example_2\n" + "THERECANBESEVERAL\n" + "LINESBUTTHEYALLMUST\n" + "BECONCATENATED";
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
