public class Main {
    static Data1[] testSet;
    static class Data1 {
        String ruleSet;
        String sample;
        String output;
        Data1(String ruleSet, String sample, String output) {
            this.ruleSet = ruleSet;
            this.sample = sample;
            this.output = output;
        }
        @Override public String toString() {
            return String.format("{'ruleSet': '%s', 'sample': '%s', 'output': '%s'}", String.valueOf(ruleSet), String.valueOf(sample), String.valueOf(output));
        }
    }


    static String[] split(String s, String sep) {
        String[] parts = new String[]{};
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (_runeLen(sep) > 0 && i + _runeLen(sep) <= _runeLen(s) && (_substr(s, i, i + _runeLen(sep)).equals(sep))) {
                parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + _runeLen(sep);
            } else {
                cur = cur + _substr(s, i, i + 1);
                i = i + 1;
            }
        }
        parts = java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return parts;
    }

    static String trimSpace(String s) {
        int start = 0;
        while (start < _runeLen(s) && ((s.substring(start, start + 1).equals(" ")) || (s.substring(start, start + 1).equals("\t")))) {
            start = start + 1;
        }
        int end = _runeLen(s);
        while (end > start && ((s.substring(end - 1, end).equals(" ")) || (s.substring(end - 1, end).equals("\t")))) {
            end = end - 1;
        }
        return s.substring(start, end);
    }

    static int indexOfSub(String s, String sub) {
        if (_runeLen(sub) == 0) {
            return 0;
        }
        int i_1 = 0;
        while (i_1 + _runeLen(sub) <= _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + _runeLen(sub)).equals(sub))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return 0 - 1;
    }

    static java.util.Map<String,Object> parseRules(String rs) {
        java.util.Map<String,Object>[] rules = (java.util.Map<String,Object>[])new java.util.Map[]{};
        for (var line : rs.split("\n")) {
            String ln = (String)(line);
            int hash = indexOfSub(ln, "#");
            if (hash >= 0) {
                ln = ln.substring(0, hash);
            }
            ln = String.valueOf(trimSpace(ln));
            if (_runeLen(ln) == 0) {
                continue;
            }
            int arrow = 0 - 1;
            int j = 0;
            while (j + 2 <= _runeLen(ln)) {
                if ((_substr(ln, j, j + 2).equals("->"))) {
                    boolean pre = j > 0 && ((ln.substring(j - 1, j).equals(" ")) || (ln.substring(j - 1, j).equals("\t")));
                    boolean post = j + 2 < _runeLen(ln) && ((ln.substring(j + 2, j + 3).equals(" ")) || (ln.substring(j + 2, j + 3).equals("\t")));
                    if (pre && post) {
                        arrow = j;
                        break;
                    }
                }
                j = j + 1;
            }
            if (arrow < 0) {
                arrow = indexOfSub(ln, "->");
            }
            if (arrow < 0) {
                return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", (Object)(false))));
            }
            String pat = String.valueOf(trimSpace(ln.substring(0, arrow)));
            String rest = String.valueOf(trimSpace(ln.substring(arrow + 2, _runeLen(ln))));
            boolean term = false;
            if (_runeLen(rest) > 0 && (rest.substring(0, 1).equals("."))) {
                term = true;
                rest = rest.substring(1, _runeLen(rest));
            }
            String rep = rest;
            rules = appendObj(rules, new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("pat", (Object)(pat)), java.util.Map.entry("rep", (Object)(rep)), java.util.Map.entry("term", (Object)(term)))));
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", true), java.util.Map.entry("rules", rules)));
    }

    static String runRules(java.util.Map<String,Object>[] rules, String s) {
        boolean changed = true;
        while (changed) {
            changed = false;
            int i_2 = 0;
            while (i_2 < rules.length) {
                java.util.Map<String,Object> r = rules[i_2];
                Object pat_1 = (Object)(((Object)(r).get("pat")));
                Object rep_1 = (Object)(((Object)(r).get("rep")));
                Object term_1 = (Object)(((Object)(r).get("term")));
                int idx = indexOfSub(s, (String)(pat_1));
                if (idx >= 0) {
                    s = s.substring(0, idx) + (String)(rep_1) + s.substring(idx + String.valueOf(pat_1).length(), _runeLen(s));
                    changed = true;
                    if (((Boolean)(term_1))) {
                        return s;
                    }
                    break;
                }
                i_2 = i_2 + 1;
            }
        }
        return s;
    }

    static java.util.Map<String,Object> interpret(String ruleset, String input) {
        java.util.Map<String,Object> p = parseRules(ruleset);
        if (!((boolean) (p.get("ok")))) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", false), java.util.Map.entry("out", "")));
        }
        String out = String.valueOf(runRules((java.util.Map<String,Object>[])(((java.util.Map<String,Object>[]) (p.get("rules")))), input));
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("ok", true), java.util.Map.entry("out", out)));
    }

    static void main() {
        System.out.println("validating " + (String)(_p(testSet.length)) + " test cases");
        boolean failures = false;
        int i_3 = 0;
        while (i_3 < testSet.length) {
            Data1 tc = testSet[i_3];
            java.util.Map<String,Object> res = interpret(tc.ruleSet, tc.sample);
            if (!((boolean) (res.get("ok")))) {
                System.out.println("test " + (String)(_p(i_3 + 1)) + " invalid ruleset");
                failures = true;
            } else             if (!(((String) (res.get("out"))).equals(tc.output))) {
                System.out.println("test " + (String)(_p(i_3 + 1)) + ": got " + ((String) (res.get("out"))) + ", want " + tc.output);
                failures = true;
            }
            i_3 = i_3 + 1;
        }
        if (!failures) {
            System.out.println("no failures");
        }
    }
    public static void main(String[] args) {
        testSet = new Data1[]{new Data1("# This rules file is extracted from Wikipedia:\n# http://en.wikipedia.org/wiki/Markov_Algorithm\nA -> apple\nB -> bag\nS -> shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule\n", "I bought a B of As from T S.", "I bought a bag of apples from my brother."), new Data1("# Slightly modified from the rules on Wikipedia\nA -> apple\nB -> bag\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule\n", "I bought a B of As from T S.", "I bought a bag of apples from T shop."), new Data1("# BNF Syntax testing rules\nA -> apple\nWWWW -> with\nBgage -> ->.*\nB -> bag\n->.* -> money\nW -> WW\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule\n", "I bought a B of As W my Bgage from T S.", "I bought a bag of apples with my money from T shop."), new Data1("### Unary Multiplication Engine, for testing Markov Algorithm implementations\n### By Donal Fellows.\n# Unary addition engine\n_+1 -> _1+\n1+1 -> 11+\n# Pass for converting from the splitting of multiplication into ordinary\n# addition\n1! -> !1\n,! -> !+\n_! -> _\n# Unary multiplication by duplicating left side, right side times\n1*1 -> x,@y\n1x -> xX\nX, -> 1,1\nX1 -> 1X\n_x -> _X\n,x -> ,X\ny1 -> 1y\ny_ -> _\n# Next phase of applying\n1@1 -> x,@y\n1@_ -> @_\n,@_ -> !_\n++ -> +\n# Termination cleanup for addition\n_1 -> 1\n1+_ -> 1\n_+_ ->\n", "_1111*11111_", "11111111111111111111"), new Data1("# Turing machine: three-state busy beaver\n#\n# state A, symbol 0 => write 1, move right, new state B\nA0 -> 1B\n# state A, symbol 1 => write 1, move left, new state C\n0A1 -> C01\n1A1 -> C11\n# state B, symbol 0 => write 1, move left, new state A\n0B0 -> A01\n1B0 -> A11\n# state B, symbol 1 => write 1, move right, new state B\nB1 -> 1B\n# state C, symbol 0 => write 1, move left, new state B\n0C0 -> B01\n1C0 -> B11\n# state C, symbol 1 => write 1, move left, halt\n0C1 -> H01\n1C1 -> H11\n", "000000A000000", "00011H1111000")};
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
