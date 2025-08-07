public class Main {
    static class Clause {
        java.util.Map<String,Integer> literals;
        String[] names;
        Clause(java.util.Map<String,Integer> literals, String[] names) {
            this.literals = literals;
            this.names = names;
        }
        Clause() {}
        @Override public String toString() {
            return String.format("{'literals': %s, 'names': %s}", String.valueOf(literals), String.valueOf(names));
        }
    }

    static class EvalResult {
        int value;
        Clause clause;
        EvalResult(int value, Clause clause) {
            this.value = value;
            this.clause = clause;
        }
        EvalResult() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'clause': %s}", String.valueOf(value), String.valueOf(clause));
        }
    }

    static class Formula {
        Clause[] clauses;
        Formula(Clause[] clauses) {
            this.clauses = clauses;
        }
        Formula() {}
        @Override public String toString() {
            return String.format("{'clauses': %s}", String.valueOf(clauses));
        }
    }

    static class DPLLResult {
        boolean sat;
        java.util.Map<String,Integer> model;
        DPLLResult(boolean sat, java.util.Map<String,Integer> model) {
            this.sat = sat;
            this.model = model;
        }
        DPLLResult() {}
        @Override public String toString() {
            return String.format("{'sat': %s, 'model': %s}", String.valueOf(sat), String.valueOf(model));
        }
    }

    static Clause clause1;
    static Clause clause2;
    static Formula formula;
    static String formula_str;
    static Clause[] clauses;
    static String[] symbols;
    static java.util.Map<String,Integer> model = null;
    static DPLLResult result;

    static Clause new_clause(String[] lits) {
        java.util.Map<String,Integer> m = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        String[] names = ((String[])(new String[]{}));
        int i = 0;
        while (i < lits.length) {
            String lit = lits[i];
m.put(lit, 0 - 1);
            names = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(names), java.util.stream.Stream.of(lit)).toArray(String[]::new)));
            i = i + 1;
        }
        return new Clause(m, names);
    }

    static Clause assign_clause(Clause c, java.util.Map<String,Integer> model) {
        java.util.Map<String,Integer> lits = c.literals;
        int i_1 = 0;
        while (i_1 < c.names.length) {
            String lit_1 = c.names[i_1];
            String symbol = _substr(lit_1, 0, 2);
            if (((Boolean)(model.containsKey(symbol)))) {
                int value = (int)(((int)(model).getOrDefault(symbol, 0)));
                if ((_substr(lit_1, _runeLen(lit_1) - 1, _runeLen(lit_1)).equals("'")) && value != 0 - 1) {
                    value = 1 - value;
                }
lits.put(lit_1, value);
            }
            i_1 = i_1 + 1;
        }
c.literals = lits;
        return c;
    }

    static EvalResult evaluate_clause(Clause c, java.util.Map<String,Integer> model) {
        int i_2 = 0;
        while (i_2 < c.names.length) {
            String lit_2 = c.names[i_2];
            String sym = String.valueOf((_substr(lit_2, _runeLen(lit_2) - 1, _runeLen(lit_2)).equals("'")) ? _substr(lit_2, 0, 2) : lit_2 + "'");
            if (((Boolean)(c.literals.containsKey(sym)))) {
                return new EvalResult(1, c);
            }
            i_2 = i_2 + 1;
        }
        c = assign_clause(c, model);
        i_2 = 0;
        while (i_2 < c.names.length) {
            String lit_3 = c.names[i_2];
            int value_1 = (int)(((int)(c.literals).getOrDefault(lit_3, 0)));
            if (value_1 == 1) {
                return new EvalResult(1, c);
            }
            if (value_1 == 0 - 1) {
                return new EvalResult(0 - 1, c);
            }
            i_2 = i_2 + 1;
        }
        int any_true = 0;
        i_2 = 0;
        while (i_2 < c.names.length) {
            String lit_4 = c.names[i_2];
            if ((int)(((int)(c.literals).getOrDefault(lit_4, 0))) == 1) {
                any_true = 1;
            }
            i_2 = i_2 + 1;
        }
        return new EvalResult(any_true, c);
    }

    static Formula new_formula(Clause[] cs) {
        return new Formula(cs);
    }

    static String[] remove_symbol(String[] symbols, String s) {
        String[] res = ((String[])(new String[]{}));
        int i_3 = 0;
        while (i_3 < symbols.length) {
            if (!(symbols[i_3].equals(s))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(symbols[i_3])).toArray(String[]::new)));
            }
            i_3 = i_3 + 1;
        }
        return res;
    }

    static DPLLResult dpll_algorithm(Clause[] clauses, String[] symbols, java.util.Map<String,Integer> model) {
        boolean all_true = true;
        int i_4 = 0;
        while (i_4 < clauses.length) {
            EvalResult ev = evaluate_clause(clauses[i_4], model);
clauses[i_4] = ev.clause;
            if (ev.value == 0) {
                return new DPLLResult(false, new java.util.LinkedHashMap<String, Integer>());
            } else             if (ev.value == 0 - 1) {
                all_true = false;
            }
            i_4 = i_4 + 1;
        }
        if (all_true) {
            return new DPLLResult(true, model);
        }
        String p = symbols[0];
        String[] rest = ((String[])(remove_symbol(((String[])(symbols)), p)));
        java.util.Map<String,Integer> tmp1 = model;
        java.util.Map<String,Integer> tmp2 = model;
tmp1.put(p, 1);
tmp2.put(p, 0);
        DPLLResult res1 = dpll_algorithm(((Clause[])(clauses)), ((String[])(rest)), tmp1);
        if (res1.sat) {
            return res1;
        }
        return dpll_algorithm(((Clause[])(clauses)), ((String[])(rest)), tmp2);
    }

    static String str_clause(Clause c) {
        String line = "{";
        boolean first = true;
        int i_5 = 0;
        while (i_5 < c.names.length) {
            String lit_5 = c.names[i_5];
            if (first) {
                first = false;
            } else {
                line = line + " , ";
            }
            line = line + lit_5;
            i_5 = i_5 + 1;
        }
        line = line + "}";
        return line;
    }

    static String str_formula(Formula f) {
        String line_1 = "{";
        int i_6 = 0;
        while (i_6 < f.clauses.length) {
            line_1 = line_1 + String.valueOf(str_clause(f.clauses[i_6]));
            if (i_6 < f.clauses.length - 1) {
                line_1 = line_1 + " , ";
            }
            i_6 = i_6 + 1;
        }
        line_1 = line_1 + "}";
        return line_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            clause1 = new_clause(((String[])(new String[]{"A4", "A3", "A5'", "A1", "A3'"})));
            clause2 = new_clause(((String[])(new String[]{"A4"})));
            formula = new_formula(((Clause[])(new Clause[]{clause1, clause2})));
            formula_str = String.valueOf(str_formula(formula));
            clauses = ((Clause[])(new Clause[]{clause1, clause2}));
            symbols = ((String[])(new String[]{"A4", "A3", "A5", "A1"}));
            model = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
            result = dpll_algorithm(((Clause[])(clauses)), ((String[])(symbols)), model);
            if (result.sat) {
                System.out.println("The formula " + formula_str + " is satisfiable.");
            } else {
                System.out.println("The formula " + formula_str + " is not satisfiable.");
            }
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
