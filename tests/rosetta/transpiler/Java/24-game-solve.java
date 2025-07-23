public class Main {
    static int OP_NUM = 0;
    static int OP_ADD = 1;
    static int OP_SUB = 2;
    static int OP_MUL = 3;
    static int OP_DIV = 4;
    static int n_cards = 4;
    static int goal = 24;
    static int digit_range = 9;

    static java.util.Map<String,Object> newNum(int n) {
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("op", OP_NUM, "value", new java.util.LinkedHashMap<String, Object>(java.util.Map.of("num", n, "denom", 1))));
    }

    static java.util.Map<String,Integer> exprEval(java.util.Map<String,Object> x) {
        if ((int)((java.util.Map)(x.get("op"))) == OP_NUM) {
            return (java.util.Map)(x.get("value"));
        }
        java.util.Map<String,Integer> l = exprEval((java.util.Map)(x.get("left")));
        java.util.Map<String,Integer> r = exprEval((java.util.Map)(x.get("right")));
        if ((int)((java.util.Map)(x.get("op"))) == OP_ADD) {
            return new java.util.LinkedHashMap<String, Integer>(java.util.Map.of("num", (int)((java.util.Map)(l.get("num"))) * (int)((java.util.Map)(r.get("denom"))) + (int)((java.util.Map)(l.get("denom"))) * (int)((java.util.Map)(r.get("num"))), "denom", (int)((java.util.Map)(l.get("denom"))) * (int)((java.util.Map)(r.get("denom")))));
        }
        if ((int)((java.util.Map)(x.get("op"))) == OP_SUB) {
            return new java.util.LinkedHashMap<String, Integer>(java.util.Map.of("num", (int)((java.util.Map)(l.get("num"))) * (int)((java.util.Map)(r.get("denom"))) - (int)((java.util.Map)(l.get("denom"))) * (int)((java.util.Map)(r.get("num"))), "denom", (int)((java.util.Map)(l.get("denom"))) * (int)((java.util.Map)(r.get("denom")))));
        }
        if ((int)((java.util.Map)(x.get("op"))) == OP_MUL) {
            return new java.util.LinkedHashMap<String, Integer>(java.util.Map.of("num", (int)((java.util.Map)(l.get("num"))) * (int)((java.util.Map)(r.get("num"))), "denom", (int)((java.util.Map)(l.get("denom"))) * (int)((java.util.Map)(r.get("denom")))));
        }
        return new java.util.LinkedHashMap<String, Integer>(java.util.Map.of("num", (int)((java.util.Map)(l.get("num"))) * (int)((java.util.Map)(r.get("denom"))), "denom", (int)((java.util.Map)(l.get("denom"))) * (int)((java.util.Map)(r.get("num")))));
    }

    static String exprString(java.util.Map<String,Object> x) {
        if ((int)((java.util.Map)(x.get("op"))) == OP_NUM) {
            return String.valueOf((java.util.Map)(x.get("value"))["num"]);
        }
        String ls = exprString((java.util.Map)(x.get("left")));
        String rs = exprString((java.util.Map)(x.get("right")));
        String opstr = "";
        if ((int)((java.util.Map)(x.get("op"))) == OP_ADD) {
            opstr = " + ";
        } else         if ((int)((java.util.Map)(x.get("op"))) == OP_SUB) {
            opstr = " - ";
        } else         if ((int)((java.util.Map)(x.get("op"))) == OP_MUL) {
            opstr = " * ";
        } else {
            opstr = " / ";
        }
        return "(" + ls + opstr + rs + ")";
    }

    static boolean solve(java.util.Map<String,Object>[] xs) {
        if (xs.length == 1) {
            java.util.Map<String,Integer> f = exprEval((java.util.Map)(xs.get(0)));
            if ((int)((int)(f.get("denom"))) != 0 && (boolean)((int)(f.get("num"))) == (int)((int)(f.get("denom"))) * goal) {
                System.out.println(exprString((java.util.Map)(xs.get(0))));
                return true;
            }
            return false;
        }
        int i = 0;
        while (i < xs.length) {
            int j = i + 1;
            while (j < xs.length) {
                java.util.Map<String,Object>[] rest = new java.util.Map<String,Object>[]{};
                int k = 0;
                while (k < xs.length) {
                    if (k != i && k != j) {
                        rest = java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of((java.util.Map)(xs.get(k)))).toArray(java.util.Map<String,Object>[]::new);
                    }
                    k = k + 1;
                }
                java.util.Map<String,Object> a = (java.util.Map<String,Object>)((java.util.Map)(xs.get(i)));
                java.util.Map<String,Object> b = (java.util.Map<String,Object>)((java.util.Map)(xs.get(j)));
                for (var op : new int[]{OP_ADD, OP_SUB, OP_MUL, OP_DIV}) {
                    java.util.Map node = new java.util.LinkedHashMap<String, Object>(java.util.Map.of("op", op, "left", a, "right", b));
                    if (solve(java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(node)).toArray(java.util.Map[]::new))) {
                        return true;
                    }
                }
                java.util.Map node = new java.util.LinkedHashMap<String, Object>(java.util.Map.of("op", OP_SUB, "left", b, "right", a));
                if (solve(java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(node)).toArray(java.util.Map[]::new))) {
                    return true;
                }
                node = new java.util.LinkedHashMap<String, Object>(java.util.Map.of("op", OP_DIV, "left", b, "right", a));
                if (solve(java.util.stream.Stream.concat(java.util.Arrays.stream(rest), java.util.stream.Stream.of(node)).toArray(java.util.Map[]::new))) {
                    return true;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return false;
    }

    static void main() {
        int iter = 0;
        while (iter < 10) {
            java.util.Map<String,Object>[] cards = new java.util.Map<String,Object>[]{};
            int i = 0;
            while (i < n_cards) {
                int n = (_now() % (digit_range - 1)) + 1;
                cards = java.util.stream.Stream.concat(java.util.Arrays.stream(cards), java.util.stream.Stream.of(newNum(n))).toArray(java.util.Map<String,Object>[]::new);
                System.out.println(" " + String.valueOf(n));
                i = i + 1;
            }
            System.out.println(":  ");
            if (!(Boolean)solve(cards)) {
                System.out.println("No solution");
            }
            iter = iter + 1;
        }
    }
    public static void main(String[] args) {
        main();
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
        return (int)System.currentTimeMillis();
    }
}
