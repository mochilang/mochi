public class Main {
    static java.math.BigInteger LABEL = java.math.BigInteger.valueOf(0);
    static java.math.BigInteger COLOR = java.math.BigInteger.valueOf(1);
    static java.math.BigInteger PARENT = java.math.BigInteger.valueOf(2);
    static java.math.BigInteger LEFT = java.math.BigInteger.valueOf(3);
    static java.math.BigInteger RIGHT = java.math.BigInteger.valueOf(4);
    static java.math.BigInteger NEG_ONE;
    static class RBTree {
        java.math.BigInteger[][] nodes;
        java.math.BigInteger root;
        RBTree(java.math.BigInteger[][] nodes, java.math.BigInteger root) {
            this.nodes = nodes;
            this.root = root;
        }
        RBTree() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'root': %s}", String.valueOf(nodes), String.valueOf(root));
        }
    }


    static RBTree make_tree() {
        return new RBTree(new java.math.BigInteger[][]{}, (java.math.BigInteger.valueOf(1)).negate());
    }

    static RBTree rotate_left(RBTree t, java.math.BigInteger x) {
        java.math.BigInteger[][] nodes = ((java.math.BigInteger[][])(t.nodes));
        java.math.BigInteger y_1 = new java.math.BigInteger(String.valueOf(nodes[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())]));
        java.math.BigInteger yLeft_1 = new java.math.BigInteger(String.valueOf(nodes[(int)(((java.math.BigInteger)(y_1)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())]));
nodes[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())] = new java.math.BigInteger(String.valueOf(yLeft_1));
        if (yLeft_1.compareTo(NEG_ONE) != 0) {
nodes[(int)(((java.math.BigInteger)(yLeft_1)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())] = new java.math.BigInteger(String.valueOf(x));
        }
        java.math.BigInteger xParent_1 = new java.math.BigInteger(String.valueOf(nodes[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
nodes[(int)(((java.math.BigInteger)(y_1)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())] = new java.math.BigInteger(String.valueOf(xParent_1));
        if (xParent_1.compareTo(NEG_ONE) == 0) {
t.root = y_1;
        } else         if (x.compareTo(nodes[(int)(((java.math.BigInteger)(xParent_1)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())]) == 0) {
nodes[(int)(((java.math.BigInteger)(xParent_1)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())] = new java.math.BigInteger(String.valueOf(y_1));
        } else {
nodes[(int)(((java.math.BigInteger)(xParent_1)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())] = new java.math.BigInteger(String.valueOf(y_1));
        }
nodes[(int)(((java.math.BigInteger)(y_1)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())] = new java.math.BigInteger(String.valueOf(x));
nodes[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())] = new java.math.BigInteger(String.valueOf(y_1));
t.nodes = nodes;
        return t;
    }

    static RBTree rotate_right(RBTree t, java.math.BigInteger x) {
        java.math.BigInteger[][] nodes_1 = ((java.math.BigInteger[][])(t.nodes));
        java.math.BigInteger y_3 = new java.math.BigInteger(String.valueOf(nodes_1[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())]));
        java.math.BigInteger yRight_1 = new java.math.BigInteger(String.valueOf(nodes_1[(int)(((java.math.BigInteger)(y_3)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())]));
nodes_1[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())] = new java.math.BigInteger(String.valueOf(yRight_1));
        if (yRight_1.compareTo(NEG_ONE) != 0) {
nodes_1[(int)(((java.math.BigInteger)(yRight_1)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())] = new java.math.BigInteger(String.valueOf(x));
        }
        java.math.BigInteger xParent_3 = new java.math.BigInteger(String.valueOf(nodes_1[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
nodes_1[(int)(((java.math.BigInteger)(y_3)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())] = new java.math.BigInteger(String.valueOf(xParent_3));
        if (xParent_3.compareTo(NEG_ONE) == 0) {
t.root = y_3;
        } else         if (x.compareTo(nodes_1[(int)(((java.math.BigInteger)(xParent_3)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())]) == 0) {
nodes_1[(int)(((java.math.BigInteger)(xParent_3)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())] = new java.math.BigInteger(String.valueOf(y_3));
        } else {
nodes_1[(int)(((java.math.BigInteger)(xParent_3)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())] = new java.math.BigInteger(String.valueOf(y_3));
        }
nodes_1[(int)(((java.math.BigInteger)(y_3)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())] = new java.math.BigInteger(String.valueOf(x));
nodes_1[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())] = new java.math.BigInteger(String.valueOf(y_3));
t.nodes = nodes_1;
        return t;
    }

    static RBTree insert_fix(RBTree t, java.math.BigInteger z) {
        java.math.BigInteger[][] nodes_2 = ((java.math.BigInteger[][])(t.nodes));
        while (z.compareTo(t.root) != 0 && nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())].compareTo(java.math.BigInteger.valueOf(1)) == 0) {
            if (nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())].compareTo(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())]) == 0) {
                java.math.BigInteger y_6 = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())]));
                if (y_6.compareTo(NEG_ONE) != 0 && nodes_2[(int)(((java.math.BigInteger)(y_6)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())].compareTo(java.math.BigInteger.valueOf(1)) == 0) {
nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(0);
nodes_2[(int)(((java.math.BigInteger)(y_6)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(0);
                    java.math.BigInteger gp_4 = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
nodes_2[(int)(((java.math.BigInteger)(gp_4)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(1);
                    z = new java.math.BigInteger(String.valueOf(gp_4));
                } else {
                    if (z.compareTo(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())]) == 0) {
                        z = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
t.nodes = nodes_2;
                        t = rotate_left(t, new java.math.BigInteger(String.valueOf(z)));
                        nodes_2 = ((java.math.BigInteger[][])(t.nodes));
                    }
nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(0);
                    java.math.BigInteger gp_5 = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
nodes_2[(int)(((java.math.BigInteger)(gp_5)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(1);
t.nodes = nodes_2;
                    t = rotate_right(t, new java.math.BigInteger(String.valueOf(gp_5)));
                    nodes_2 = ((java.math.BigInteger[][])(t.nodes));
                }
            } else {
                java.math.BigInteger y_7 = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())]));
                if (y_7.compareTo(NEG_ONE) != 0 && nodes_2[(int)(((java.math.BigInteger)(y_7)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())].compareTo(java.math.BigInteger.valueOf(1)) == 0) {
nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(0);
nodes_2[(int)(((java.math.BigInteger)(y_7)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(0);
                    java.math.BigInteger gp_6 = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
nodes_2[(int)(((java.math.BigInteger)(gp_6)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(1);
                    z = new java.math.BigInteger(String.valueOf(gp_6));
                } else {
                    if (z.compareTo(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())]) == 0) {
                        z = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
t.nodes = nodes_2;
                        t = rotate_right(t, new java.math.BigInteger(String.valueOf(z)));
                        nodes_2 = ((java.math.BigInteger[][])(t.nodes));
                    }
nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(0);
                    java.math.BigInteger gp_7 = new java.math.BigInteger(String.valueOf(nodes_2[(int)(((java.math.BigInteger)(nodes_2[(int)(((java.math.BigInteger)(z)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())])).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())]));
nodes_2[(int)(((java.math.BigInteger)(gp_7)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(1);
t.nodes = nodes_2;
                    t = rotate_left(t, new java.math.BigInteger(String.valueOf(gp_7)));
                    nodes_2 = ((java.math.BigInteger[][])(t.nodes));
                }
            }
        }
        nodes_2 = ((java.math.BigInteger[][])(t.nodes));
nodes_2[(int)(((java.math.BigInteger)(t.root)).longValue())][(int)(((java.math.BigInteger)(COLOR)).longValue())] = java.math.BigInteger.valueOf(0);
t.nodes = nodes_2;
        return t;
    }

    static RBTree tree_insert(RBTree t, java.math.BigInteger v) {
        java.math.BigInteger[][] nodes_3 = ((java.math.BigInteger[][])(t.nodes));
        java.math.BigInteger[] node_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(v)), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}));
        nodes_3 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_3), java.util.stream.Stream.of(new java.math.BigInteger[][]{node_1})).toArray(java.math.BigInteger[][]::new)));
        java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(nodes_3.length)).subtract(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger y_9 = new java.math.BigInteger(String.valueOf(NEG_ONE));
        java.math.BigInteger x_1 = new java.math.BigInteger(String.valueOf(t.root));
        while (x_1.compareTo(NEG_ONE) != 0) {
            y_9 = new java.math.BigInteger(String.valueOf(x_1));
            if (v.compareTo(nodes_3[(int)(((java.math.BigInteger)(x_1)).longValue())][(int)(((java.math.BigInteger)(LABEL)).longValue())]) < 0) {
                x_1 = new java.math.BigInteger(String.valueOf(nodes_3[(int)(((java.math.BigInteger)(x_1)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())]));
            } else {
                x_1 = new java.math.BigInteger(String.valueOf(nodes_3[(int)(((java.math.BigInteger)(x_1)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())]));
            }
        }
nodes_3[(int)(((java.math.BigInteger)(idx_1)).longValue())][(int)(((java.math.BigInteger)(PARENT)).longValue())] = new java.math.BigInteger(String.valueOf(y_9));
        if (y_9.compareTo(NEG_ONE) == 0) {
t.root = idx_1;
        } else         if (v.compareTo(nodes_3[(int)(((java.math.BigInteger)(y_9)).longValue())][(int)(((java.math.BigInteger)(LABEL)).longValue())]) < 0) {
nodes_3[(int)(((java.math.BigInteger)(y_9)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())] = new java.math.BigInteger(String.valueOf(idx_1));
        } else {
nodes_3[(int)(((java.math.BigInteger)(y_9)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())] = new java.math.BigInteger(String.valueOf(idx_1));
        }
t.nodes = nodes_3;
        t = insert_fix(t, new java.math.BigInteger(String.valueOf(idx_1)));
        return t;
    }

    static java.math.BigInteger[] inorder(RBTree t, java.math.BigInteger x, java.math.BigInteger[] acc) {
        if (x.compareTo(NEG_ONE) == 0) {
            return acc;
        }
        acc = ((java.math.BigInteger[])(inorder(t, new java.math.BigInteger(String.valueOf(t.nodes[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(LEFT)).longValue())])), ((java.math.BigInteger[])(acc)))));
        acc = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(acc), java.util.stream.Stream.of(t.nodes[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(LABEL)).longValue())])).toArray(java.math.BigInteger[]::new)));
        acc = ((java.math.BigInteger[])(inorder(t, new java.math.BigInteger(String.valueOf(t.nodes[(int)(((java.math.BigInteger)(x)).longValue())][(int)(((java.math.BigInteger)(RIGHT)).longValue())])), ((java.math.BigInteger[])(acc)))));
        return acc;
    }

    static void main() {
        RBTree t = make_tree();
        java.math.BigInteger[] values_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(20), java.math.BigInteger.valueOf(30), java.math.BigInteger.valueOf(15), java.math.BigInteger.valueOf(25), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(1)}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(values_1.length))) < 0) {
            t = tree_insert(t, new java.math.BigInteger(String.valueOf(values_1[(int)(((java.math.BigInteger)(i_1)).longValue())])));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] res_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        res_1 = ((java.math.BigInteger[])(inorder(t, new java.math.BigInteger(String.valueOf(t.root)), ((java.math.BigInteger[])(res_1)))));
        System.out.println(_p(res_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NEG_ONE = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
            main();
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
