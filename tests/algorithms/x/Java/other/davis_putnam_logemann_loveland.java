public class Main {
    static class Clause {
        java.util.Map<String,Long> literals;
        String[] names;
        Clause(java.util.Map<String,Long> literals, String[] names) {
            this.literals = literals;
            this.names = names;
        }
        Clause() {}
        @Override public String toString() {
            return String.format("{'literals': %s, 'names': %s}", String.valueOf(literals), String.valueOf(names));
        }
    }

    static class EvalResult {
        long value;
        Clause clause;
        EvalResult(long value, Clause clause) {
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
        java.util.Map<String,Long> model;
        DPLLResult(boolean sat, java.util.Map<String,Long> model) {
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
    static String[] symbols = ((String[])(new String[]{"A4", "A3", "A5", "A1"}));
    static java.util.Map<String,Long> model = null;
    static DPLLResult result;

    static Clause new_clause(String[] lits) {
        java.util.Map<String,Long> m = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        String[] names_1 = ((String[])(new String[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(lits.length)) {
            String lit_1 = lits[(int)((long)(i_1))];
m.put(lit_1, (long)(-1));
            names_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(names_1), java.util.stream.Stream.of(lit_1)).toArray(String[]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new Clause(m, names_1);
    }

    static Clause assign_clause(Clause c, java.util.Map<String,Long> model) {
        java.util.Map<String,Long> lits = c.literals;
        long i_3 = 0L;
        while ((long)(i_3) < (long)(c.names.length)) {
            String lit_3 = c.names[(int)((long)(i_3))];
            String symbol_1 = _substr(lit_3, (int)(0L), (int)(2L));
            if (model.containsKey(symbol_1)) {
                long value_1 = (long)(((long)(model).getOrDefault(symbol_1, 0L)));
                if ((_substr(lit_3, (int)((long)((long)(_runeLen(lit_3)) - 1L)), (int)((long)(_runeLen(lit_3)))).equals("'")) && (long)(value_1) != (long)(-1)) {
                    value_1 = (long)(1L - (long)(value_1));
                }
lits.put(lit_3, (long)(value_1));
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
c.literals = lits;
        return c;
    }

    static EvalResult evaluate_clause(Clause c, java.util.Map<String,Long> model) {
        long i_4 = 0L;
        while ((long)(i_4) < (long)(c.names.length)) {
            String lit_5 = c.names[(int)((long)(i_4))];
            String sym_1 = String.valueOf((_substr(lit_5, (int)((long)((long)(_runeLen(lit_5)) - 1L)), (int)((long)(_runeLen(lit_5)))).equals("'")) ? _substr(lit_5, (int)(0L), (int)(2L)) : lit_5 + "'");
            if (c.literals.containsKey(sym_1)) {
                return new EvalResult(1, c);
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        c = assign_clause(c, model);
        i_4 = 0L;
        while ((long)(i_4) < (long)(c.names.length)) {
            String lit_7 = c.names[(int)((long)(i_4))];
            long value_3 = (long)(((long)(c.literals).getOrDefault(lit_7, 0L)));
            if ((long)(value_3) == 1L) {
                return new EvalResult(1, c);
            }
            if ((long)(value_3) == (long)(-1)) {
                return new EvalResult(-1, c);
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        long any_true_1 = 0L;
        i_4 = 0L;
        while ((long)(i_4) < (long)(c.names.length)) {
            String lit_9 = c.names[(int)((long)(i_4))];
            if ((long)(((long)(c.literals).getOrDefault(lit_9, 0L))) == 1L) {
                any_true_1 = 1L;
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        return new EvalResult(any_true_1, c);
    }

    static Formula new_formula(Clause[] cs) {
        return new Formula(cs);
    }

    static String[] remove_symbol(String[] symbols, String s) {
        String[] res = ((String[])(new String[]{}));
        long i_6 = 0L;
        while ((long)(i_6) < (long)(symbols.length)) {
            if (!(symbols[(int)((long)(i_6))].equals(s))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(symbols[(int)((long)(i_6))])).toArray(String[]::new)));
            }
            i_6 = (long)((long)(i_6) + 1L);
        }
        return res;
    }

    static DPLLResult dpll_algorithm(Clause[] clauses, String[] symbols, java.util.Map<String,Long> model) {
        boolean all_true = true;
        long i_8 = 0L;
        while ((long)(i_8) < (long)(clauses.length)) {
            EvalResult ev_1 = evaluate_clause(clauses[(int)((long)(i_8))], model);
clauses[(int)((long)(i_8))] = ev_1.clause;
            if ((long)(ev_1.value) == 0L) {
                return new DPLLResult(false, new java.util.LinkedHashMap<String, Long>());
            } else             if ((long)(ev_1.value) == (long)(-1)) {
                all_true = false;
            }
            i_8 = (long)((long)(i_8) + 1L);
        }
        if (all_true) {
            return new DPLLResult(true, model);
        }
        String p_1 = symbols[(int)(0L)];
        String[] rest_1 = ((String[])(remove_symbol(((String[])(symbols)), p_1)));
        java.util.Map<String,Long> tmp1_1 = model;
        java.util.Map<String,Long> tmp2_1 = model;
tmp1_1.put(p_1, 1L);
tmp2_1.put(p_1, 0L);
        DPLLResult res1_1 = dpll_algorithm(((Clause[])(clauses)), ((String[])(rest_1)), tmp1_1);
        if (res1_1.sat) {
            return res1_1;
        }
        return dpll_algorithm(((Clause[])(clauses)), ((String[])(rest_1)), tmp2_1);
    }

    static String str_clause(Clause c) {
        String line = "{";
        boolean first_1 = true;
        long i_10 = 0L;
        while ((long)(i_10) < (long)(c.names.length)) {
            String lit_11 = c.names[(int)((long)(i_10))];
            if (first_1) {
                first_1 = false;
            } else {
                line = line + " , ";
            }
            line = line + lit_11;
            i_10 = (long)((long)(i_10) + 1L);
        }
        line = line + "}";
        return line;
    }

    static String str_formula(Formula f) {
        String line_1 = "{";
        long i_12 = 0L;
        while ((long)(i_12) < (long)(f.clauses.length)) {
            line_1 = line_1 + String.valueOf(str_clause(f.clauses[(int)((long)(i_12))]));
            if ((long)(i_12) < (long)((long)(f.clauses.length) - 1L)) {
                line_1 = line_1 + " , ";
            }
            i_12 = (long)((long)(i_12) + 1L);
        }
        line_1 = line_1 + "}";
        return line_1;
    }
    public static void main(String[] args) {
        clause1 = new_clause(((String[])(new String[]{"A4", "A3", "A5'", "A1", "A3'"})));
        clause2 = new_clause(((String[])(new String[]{"A4"})));
        formula = new_formula(((Clause[])(new Clause[]{clause1, clause2})));
        formula_str = String.valueOf(str_formula(formula));
        clauses = ((Clause[])(new Clause[]{clause1, clause2}));
        model = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        result = dpll_algorithm(((Clause[])(clauses)), ((String[])(symbols)), model);
        if (result.sat) {
            System.out.println("The formula " + formula_str + " is satisfiable.");
        } else {
            System.out.println("The formula " + formula_str + " is not satisfiable.");
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
