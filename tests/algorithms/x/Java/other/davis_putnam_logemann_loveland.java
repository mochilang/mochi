public class Main {
    static class Clause {
        java.util.Map<String,java.math.BigInteger> literals;
        String[] names;
        Clause(java.util.Map<String,java.math.BigInteger> literals, String[] names) {
            this.literals = literals;
            this.names = names;
        }
        Clause() {}
        @Override public String toString() {
            return String.format("{'literals': %s, 'names': %s}", String.valueOf(literals), String.valueOf(names));
        }
    }

    static class EvalResult {
        java.math.BigInteger value;
        Clause clause;
        EvalResult(java.math.BigInteger value, Clause clause) {
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
        java.util.Map<String,java.math.BigInteger> model;
        DPLLResult(boolean sat, java.util.Map<String,java.math.BigInteger> model) {
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
    static java.util.Map<String,java.math.BigInteger> model = null;
    static DPLLResult result;

    static Clause new_clause(String[] lits) {
        java.util.Map<String,java.math.BigInteger> m = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>()));
        String[] names_1 = ((String[])(new String[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(lits.length))) < 0) {
            String lit_1 = lits[_idx((lits).length, ((java.math.BigInteger)(i_1)).longValue())];
m.put(lit_1, new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())));
            names_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(names_1), java.util.stream.Stream.of(lit_1)).toArray(String[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new Clause(m, ((String[])(names_1)));
    }

    static Clause assign_clause(Clause c, java.util.Map<String,java.math.BigInteger> model) {
        java.util.Map<String,java.math.BigInteger> lits = c.literals;
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(c.names.length))) < 0) {
            String lit_3 = c.names[_idx((c.names).length, ((java.math.BigInteger)(i_3)).longValue())];
            String symbol_1 = _substr(lit_3, (int)(0L), (int)(2L));
            if (model.containsKey(symbol_1)) {
                java.math.BigInteger value_1 = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(model).get(symbol_1))));
                if ((_substr(lit_3, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(lit_3))).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(lit_3)))).equals("'")) && value_1.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0) {
                    value_1 = new java.math.BigInteger(String.valueOf(java.math.BigInteger.valueOf(1).subtract(value_1)));
                }
lits.put(lit_3, new java.math.BigInteger(String.valueOf(value_1)));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
c.literals = lits;
        return c;
    }

    static EvalResult evaluate_clause(Clause c, java.util.Map<String,java.math.BigInteger> model) {
        java.math.BigInteger i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(c.names.length))) < 0) {
            String lit_5 = c.names[_idx((c.names).length, ((java.math.BigInteger)(i_4)).longValue())];
            String sym_1 = String.valueOf((_substr(lit_5, (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(_runeLen(lit_5))).subtract(java.math.BigInteger.valueOf(1)))).longValue()), (int)((long)(_runeLen(lit_5)))).equals("'")) ? _substr(lit_5, (int)(0L), (int)(2L)) : lit_5 + "'");
            if (c.literals.containsKey(sym_1)) {
                return new EvalResult(java.math.BigInteger.valueOf(1), c);
            }
            i_4 = new java.math.BigInteger(String.valueOf(i_4.add(java.math.BigInteger.valueOf(1))));
        }
        c = assign_clause(c, model);
        i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(c.names.length))) < 0) {
            String lit_7 = c.names[_idx((c.names).length, ((java.math.BigInteger)(i_4)).longValue())];
            java.math.BigInteger value_3 = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(c.literals).get(lit_7))));
            if (value_3.compareTo(java.math.BigInteger.valueOf(1)) == 0) {
                return new EvalResult(java.math.BigInteger.valueOf(1), c);
            }
            if (value_3.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
                return new EvalResult(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), c);
            }
            i_4 = new java.math.BigInteger(String.valueOf(i_4.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger any_true_1 = java.math.BigInteger.valueOf(0);
        i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(c.names.length))) < 0) {
            String lit_9 = c.names[_idx((c.names).length, ((java.math.BigInteger)(i_4)).longValue())];
            if (((java.math.BigInteger)(c.literals).get(lit_9)).compareTo(java.math.BigInteger.valueOf(1)) == 0) {
                any_true_1 = java.math.BigInteger.valueOf(1);
            }
            i_4 = new java.math.BigInteger(String.valueOf(i_4.add(java.math.BigInteger.valueOf(1))));
        }
        return new EvalResult(new java.math.BigInteger(String.valueOf(any_true_1)), c);
    }

    static Formula new_formula(Clause[] cs) {
        return new Formula(((Clause[])(cs)));
    }

    static String[] remove_symbol(String[] symbols, String s) {
        String[] res = ((String[])(new String[]{}));
        java.math.BigInteger i_6 = java.math.BigInteger.valueOf(0);
        while (i_6.compareTo(new java.math.BigInteger(String.valueOf(symbols.length))) < 0) {
            if (!(symbols[_idx((symbols).length, ((java.math.BigInteger)(i_6)).longValue())].equals(s))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(symbols[_idx((symbols).length, ((java.math.BigInteger)(i_6)).longValue())])).toArray(String[]::new)));
            }
            i_6 = new java.math.BigInteger(String.valueOf(i_6.add(java.math.BigInteger.valueOf(1))));
        }
        return ((String[])(res));
    }

    static DPLLResult dpll_algorithm(Clause[] clauses, String[] symbols, java.util.Map<String,java.math.BigInteger> model) {
        boolean all_true = true;
        java.math.BigInteger i_8 = java.math.BigInteger.valueOf(0);
        while (i_8.compareTo(new java.math.BigInteger(String.valueOf(clauses.length))) < 0) {
            EvalResult ev_1 = evaluate_clause(clauses[_idx((clauses).length, ((java.math.BigInteger)(i_8)).longValue())], model);
clauses[(int)(((java.math.BigInteger)(i_8)).longValue())] = ev_1.clause;
            if (ev_1.value.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                return new DPLLResult(false, ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>())));
            } else             if (ev_1.value.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
                all_true = false;
            }
            i_8 = new java.math.BigInteger(String.valueOf(i_8.add(java.math.BigInteger.valueOf(1))));
        }
        if (all_true) {
            return new DPLLResult(true, model);
        }
        String p_1 = symbols[_idx((symbols).length, 0L)];
        String[] rest_1 = ((String[])(remove_symbol(((String[])(symbols)), p_1)));
        java.util.Map<String,java.math.BigInteger> tmp1_1 = model;
        java.util.Map<String,java.math.BigInteger> tmp2_1 = model;
tmp1_1.put(p_1, java.math.BigInteger.valueOf(1));
tmp2_1.put(p_1, java.math.BigInteger.valueOf(0));
        DPLLResult res1_1 = dpll_algorithm(((Clause[])(clauses)), ((String[])(rest_1)), tmp1_1);
        if (res1_1.sat) {
            return res1_1;
        }
        return dpll_algorithm(((Clause[])(clauses)), ((String[])(rest_1)), tmp2_1);
    }

    static String str_clause(Clause c) {
        String line = "{";
        boolean first_1 = true;
        java.math.BigInteger i_10 = java.math.BigInteger.valueOf(0);
        while (i_10.compareTo(new java.math.BigInteger(String.valueOf(c.names.length))) < 0) {
            String lit_11 = c.names[_idx((c.names).length, ((java.math.BigInteger)(i_10)).longValue())];
            if (first_1) {
                first_1 = false;
            } else {
                line = line + " , ";
            }
            line = line + lit_11;
            i_10 = new java.math.BigInteger(String.valueOf(i_10.add(java.math.BigInteger.valueOf(1))));
        }
        line = line + "}";
        return line;
    }

    static String str_formula(Formula f) {
        String line_1 = "{";
        java.math.BigInteger i_12 = java.math.BigInteger.valueOf(0);
        while (i_12.compareTo(new java.math.BigInteger(String.valueOf(f.clauses.length))) < 0) {
            line_1 = line_1 + String.valueOf(str_clause(f.clauses[_idx((f.clauses).length, ((java.math.BigInteger)(i_12)).longValue())]));
            if (i_12.compareTo(new java.math.BigInteger(String.valueOf(f.clauses.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                line_1 = line_1 + " , ";
            }
            i_12 = new java.math.BigInteger(String.valueOf(i_12.add(java.math.BigInteger.valueOf(1))));
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
            model = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>()));
            result = dpll_algorithm(((Clause[])(clauses)), ((String[])(symbols)), model);
            if (result.sat) {
                System.out.println("The formula " + formula_str + " is satisfiable.");
            } else {
                System.out.println("The formula " + formula_str + " is not satisfiable.");
            }
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        int len = _runeLen(s);
        if (i < 0) i = 0;
        if (j > len) j = len;
        if (i > j) i = j;
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
