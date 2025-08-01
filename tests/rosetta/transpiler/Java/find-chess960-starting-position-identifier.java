public class Main {
    static String glyphs;
    static java.util.Map<String,String> g2lMap;
    static java.util.Map<String,String> names;
    static java.util.Map<String,Integer> ntable;

    static int indexOf(String s, String sub) {
        int i = 0;
        while (i <= _runeLen(s) - _runeLen(sub)) {
            if ((_substr(s, i, i + _runeLen(sub)).equals(sub))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String strReplace(String s, String old, String new_) {
        String res = "";
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if (_runeLen(old) > 0 && i_1 + _runeLen(old) <= _runeLen(s) && (_substr(s, i_1, i_1 + _runeLen(old)).equals(old))) {
                res = res + new_;
                i_1 = i_1 + _runeLen(old);
            } else {
                res = res + _substr(s, i_1, i_1 + 1);
                i_1 = i_1 + 1;
            }
        }
        return res;
    }

    static String g2l(String pieces) {
        String lets = "";
        int i_2 = 0;
        while (i_2 < _runeLen(pieces)) {
            String ch = _substr(pieces, i_2, i_2 + 1);
            lets = lets + ((String)(g2lMap).get(ch));
            i_2 = i_2 + 1;
        }
        return lets;
    }

    static int spid(String pieces) {
        pieces = String.valueOf(g2l(pieces));
        if (_runeLen(pieces) != 8) {
            return -1;
        }
        for (String one : new String[]{"K", "Q"}) {
            int count = 0;
            int i_3 = 0;
            while (i_3 < _runeLen(pieces)) {
                if ((_substr(pieces, i_3, i_3 + 1).equals(one))) {
                    count = count + 1;
                }
                i_3 = i_3 + 1;
            }
            if (count != 1) {
                return -1;
            }
        }
        for (String two : new String[]{"R", "N", "B"}) {
            int count_1 = 0;
            int i_4 = 0;
            while (i_4 < _runeLen(pieces)) {
                if ((_substr(pieces, i_4, i_4 + 1).equals(two))) {
                    count_1 = count_1 + 1;
                }
                i_4 = i_4 + 1;
            }
            if (count_1 != 2) {
                return -1;
            }
        }
        int r1 = ((Number)(pieces.indexOf("R"))).intValue();
        int r2 = ((Number)(_substr(pieces, r1 + 1, _runeLen(pieces)).indexOf("R"))).intValue() + r1 + 1;
        int k = ((Number)(pieces.indexOf("K"))).intValue();
        if (k < r1 || k > r2) {
            return -1;
        }
        int b1 = ((Number)(pieces.indexOf("B"))).intValue();
        int b2 = ((Number)(_substr(pieces, b1 + 1, _runeLen(pieces)).indexOf("B"))).intValue() + b1 + 1;
        if (Math.floorMod((b2 - b1), 2) == 0) {
            return -1;
        }
        String piecesN = String.valueOf(strReplace(pieces, "Q", ""));
        piecesN = String.valueOf(strReplace(piecesN, "B", ""));
        int n1 = ((Number)(piecesN.indexOf("N"))).intValue();
        int n2 = ((Number)(_substr(piecesN, n1 + 1, _runeLen(piecesN)).indexOf("N"))).intValue() + n1 + 1;
        String np = String.valueOf(n1) + String.valueOf(n2);
        int N = (int)(((int)(ntable).get(np)));
        String piecesQ = String.valueOf(strReplace(pieces, "B", ""));
        int Q = ((Number)(piecesQ.indexOf("Q"))).intValue();
        int D = ((Number)("0246".indexOf(String.valueOf(b1)))).intValue();
        int L = ((Number)("1357".indexOf(String.valueOf(b2)))).intValue();
        if (D == (0 - 1)) {
            D = ((Number)("0246".indexOf(String.valueOf(b2)))).intValue();
            L = ((Number)("1357".indexOf(String.valueOf(b1)))).intValue();
        }
        return 96 * N + 16 * Q + 4 * D + L;
    }

    static void main() {
        for (String pieces : new String[]{"♕♘♖♗♗♘♔♖", "♖♘♗♕♔♗♘♖", "♖♕♘♗♗♔♖♘", "♖♘♕♗♗♔♖♘"}) {
            System.out.println(pieces + " or " + String.valueOf(g2l(pieces)) + " has SP-ID of " + String.valueOf(spid(pieces)));
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            glyphs = "♜♞♝♛♚♖♘♗♕♔";
            g2lMap = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("♜", "R"), java.util.Map.entry("♞", "N"), java.util.Map.entry("♝", "B"), java.util.Map.entry("♛", "Q"), java.util.Map.entry("♚", "K"), java.util.Map.entry("♖", "R"), java.util.Map.entry("♘", "N"), java.util.Map.entry("♗", "B"), java.util.Map.entry("♕", "Q"), java.util.Map.entry("♔", "K")))));
            names = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("R", "rook"), java.util.Map.entry("N", "knight"), java.util.Map.entry("B", "bishop"), java.util.Map.entry("Q", "queen"), java.util.Map.entry("K", "king")))));
            ntable = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("01", 0), java.util.Map.entry("02", 1), java.util.Map.entry("03", 2), java.util.Map.entry("04", 3), java.util.Map.entry("12", 4), java.util.Map.entry("13", 5), java.util.Map.entry("14", 6), java.util.Map.entry("23", 7), java.util.Map.entry("24", 8), java.util.Map.entry("34", 9)))));
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
