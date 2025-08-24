public class Main {
    static class Tree {
        java.math.BigInteger[] values;
        java.math.BigInteger[] lefts;
        java.math.BigInteger[] rights;
        java.math.BigInteger root;
        Tree(java.math.BigInteger[] values, java.math.BigInteger[] lefts, java.math.BigInteger[] rights, java.math.BigInteger root) {
            this.values = values;
            this.lefts = lefts;
            this.rights = rights;
            this.root = root;
        }
        Tree() {}
        @Override public String toString() {
            return String.format("{'values': %s, 'lefts': %s, 'rights': %s, 'root': %s}", String.valueOf(values), String.valueOf(lefts), String.valueOf(rights), String.valueOf(root));
        }
    }

    static java.math.BigInteger NIL;
    static class Pair {
        java.math.BigInteger idx;
        java.math.BigInteger hd;
        Pair(java.math.BigInteger idx, java.math.BigInteger hd) {
            this.idx = idx;
            this.hd = hd;
        }
        Pair() {}
        @Override public String toString() {
            return String.format("{'idx': %s, 'hd': %s}", String.valueOf(idx), String.valueOf(hd));
        }
    }

    static Tree tree;

    static Tree make_tree() {
        return new Tree(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(20), java.math.BigInteger.valueOf(15), java.math.BigInteger.valueOf(7)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(NIL)), java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf(NIL)), new java.math.BigInteger(String.valueOf(NIL))}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf(NIL)), java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf(NIL)), new java.math.BigInteger(String.valueOf(NIL))}, 0);
    }

    static java.math.BigInteger index_of(java.math.BigInteger[] xs, java.math.BigInteger x) {
        java.math.BigInteger i = java.math.BigInteger.valueOf(0);
        while (i.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            if (xs[(int)(((java.math.BigInteger)(i)).longValue())].compareTo(x) == 0) {
                return i;
            }
            i = new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1))));
        }
        return NIL;
    }

    static void sort_pairs(java.math.BigInteger[] hds, java.math.BigInteger[] vals) {
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(hds.length))) < 0) {
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(i_1));
            while (j_1.compareTo(java.math.BigInteger.valueOf(0)) > 0 && hds[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())].compareTo(hds[(int)(((java.math.BigInteger)(j_1)).longValue())]) > 0) {
                java.math.BigInteger hd_tmp_1 = new java.math.BigInteger(String.valueOf(hds[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
hds[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())] = new java.math.BigInteger(String.valueOf(hds[(int)(((java.math.BigInteger)(j_1)).longValue())]));
hds[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(hd_tmp_1));
                java.math.BigInteger val_tmp_1 = new java.math.BigInteger(String.valueOf(vals[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
vals[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())] = new java.math.BigInteger(String.valueOf(vals[(int)(((java.math.BigInteger)(j_1)).longValue())]));
vals[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(val_tmp_1));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
    }

    static java.math.BigInteger[] right_view(Tree t) {
        java.math.BigInteger[] res = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] queue_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(t.root))}));
        while (new java.math.BigInteger(String.valueOf(queue_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger size_1 = new java.math.BigInteger(String.valueOf(queue_1.length));
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
            while (i_3.compareTo(size_1) < 0) {
                java.math.BigInteger idx_1 = new java.math.BigInteger(String.valueOf(queue_1[(int)(((java.math.BigInteger)(i_3)).longValue())]));
                if (t.lefts[(int)(((java.math.BigInteger)(idx_1)).longValue())].compareTo(NIL) != 0) {
                    queue_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(t.lefts[(int)(((java.math.BigInteger)(idx_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
                }
                if (t.rights[(int)(((java.math.BigInteger)(idx_1)).longValue())].compareTo(NIL) != 0) {
                    queue_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(t.rights[(int)(((java.math.BigInteger)(idx_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
                }
                i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            }
            res = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(t.values[(int)(((java.math.BigInteger)(queue_1[(int)(((java.math.BigInteger)(size_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())])).longValue())])).toArray(java.math.BigInteger[]::new)));
            queue_1 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(queue_1, (int)(((java.math.BigInteger)(size_1)).longValue()), (int)((long)(queue_1.length)))));
        }
        return res;
    }

    static java.math.BigInteger[] left_view(Tree t) {
        java.math.BigInteger[] res_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] queue_3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(t.root))}));
        while (new java.math.BigInteger(String.valueOf(queue_3.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger size_3 = new java.math.BigInteger(String.valueOf(queue_3.length));
            java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
            while (i_5.compareTo(size_3) < 0) {
                java.math.BigInteger idx_3 = new java.math.BigInteger(String.valueOf(queue_3[(int)(((java.math.BigInteger)(i_5)).longValue())]));
                if (t.lefts[(int)(((java.math.BigInteger)(idx_3)).longValue())].compareTo(NIL) != 0) {
                    queue_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_3), java.util.stream.Stream.of(t.lefts[(int)(((java.math.BigInteger)(idx_3)).longValue())])).toArray(java.math.BigInteger[]::new)));
                }
                if (t.rights[(int)(((java.math.BigInteger)(idx_3)).longValue())].compareTo(NIL) != 0) {
                    queue_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_3), java.util.stream.Stream.of(t.rights[(int)(((java.math.BigInteger)(idx_3)).longValue())])).toArray(java.math.BigInteger[]::new)));
                }
                i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
            }
            res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(t.values[(int)(((java.math.BigInteger)(queue_3[(int)(0L)])).longValue())])).toArray(java.math.BigInteger[]::new)));
            queue_3 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(queue_3, (int)(((java.math.BigInteger)(size_3)).longValue()), (int)((long)(queue_3.length)))));
        }
        return res_1;
    }

    static java.math.BigInteger[] top_view(Tree t) {
        java.math.BigInteger[] hds = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] vals_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] queue_idx_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(t.root))}));
        java.math.BigInteger[] queue_hd_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}));
        while (new java.math.BigInteger(String.valueOf(queue_idx_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger idx_5 = new java.math.BigInteger(String.valueOf(queue_idx_1[(int)(0L)]));
            queue_idx_1 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(queue_idx_1, (int)(1L), (int)((long)(queue_idx_1.length)))));
            java.math.BigInteger hd_1 = new java.math.BigInteger(String.valueOf(queue_hd_1[(int)(0L)]));
            queue_hd_1 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(queue_hd_1, (int)(1L), (int)((long)(queue_hd_1.length)))));
            if (index_of(((java.math.BigInteger[])(hds)), new java.math.BigInteger(String.valueOf(hd_1))).compareTo(NIL) == 0) {
                hds = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(hds), java.util.stream.Stream.of(hd_1)).toArray(java.math.BigInteger[]::new)));
                vals_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_1), java.util.stream.Stream.of(t.values[(int)(((java.math.BigInteger)(idx_5)).longValue())])).toArray(java.math.BigInteger[]::new)));
            }
            if (t.lefts[(int)(((java.math.BigInteger)(idx_5)).longValue())].compareTo(NIL) != 0) {
                queue_idx_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_idx_1), java.util.stream.Stream.of(t.lefts[(int)(((java.math.BigInteger)(idx_5)).longValue())])).toArray(java.math.BigInteger[]::new)));
                queue_hd_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_hd_1), java.util.stream.Stream.of(hd_1.subtract(java.math.BigInteger.valueOf(1)))).toArray(java.math.BigInteger[]::new)));
            }
            if (t.rights[(int)(((java.math.BigInteger)(idx_5)).longValue())].compareTo(NIL) != 0) {
                queue_idx_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_idx_1), java.util.stream.Stream.of(t.rights[(int)(((java.math.BigInteger)(idx_5)).longValue())])).toArray(java.math.BigInteger[]::new)));
                queue_hd_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_hd_1), java.util.stream.Stream.of(hd_1.add(java.math.BigInteger.valueOf(1)))).toArray(java.math.BigInteger[]::new)));
            }
        }
        sort_pairs(((java.math.BigInteger[])(hds)), ((java.math.BigInteger[])(vals_1)));
        return vals_1;
    }

    static java.math.BigInteger[] bottom_view(Tree t) {
        java.math.BigInteger[] hds_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] vals_3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] queue_idx_3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(t.root))}));
        java.math.BigInteger[] queue_hd_3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}));
        while (new java.math.BigInteger(String.valueOf(queue_idx_3.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger idx_7 = new java.math.BigInteger(String.valueOf(queue_idx_3[(int)(0L)]));
            queue_idx_3 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(queue_idx_3, (int)(1L), (int)((long)(queue_idx_3.length)))));
            java.math.BigInteger hd_3 = new java.math.BigInteger(String.valueOf(queue_hd_3[(int)(0L)]));
            queue_hd_3 = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(queue_hd_3, (int)(1L), (int)((long)(queue_hd_3.length)))));
            java.math.BigInteger pos_1 = new java.math.BigInteger(String.valueOf(index_of(((java.math.BigInteger[])(hds_1)), new java.math.BigInteger(String.valueOf(hd_3)))));
            if (pos_1.compareTo(NIL) == 0) {
                hds_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(hds_1), java.util.stream.Stream.of(hd_3)).toArray(java.math.BigInteger[]::new)));
                vals_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(vals_3), java.util.stream.Stream.of(t.values[(int)(((java.math.BigInteger)(idx_7)).longValue())])).toArray(java.math.BigInteger[]::new)));
            } else {
vals_3[(int)(((java.math.BigInteger)(pos_1)).longValue())] = new java.math.BigInteger(String.valueOf(t.values[(int)(((java.math.BigInteger)(idx_7)).longValue())]));
            }
            if (t.lefts[(int)(((java.math.BigInteger)(idx_7)).longValue())].compareTo(NIL) != 0) {
                queue_idx_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_idx_3), java.util.stream.Stream.of(t.lefts[(int)(((java.math.BigInteger)(idx_7)).longValue())])).toArray(java.math.BigInteger[]::new)));
                queue_hd_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_hd_3), java.util.stream.Stream.of(hd_3.subtract(java.math.BigInteger.valueOf(1)))).toArray(java.math.BigInteger[]::new)));
            }
            if (t.rights[(int)(((java.math.BigInteger)(idx_7)).longValue())].compareTo(NIL) != 0) {
                queue_idx_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_idx_3), java.util.stream.Stream.of(t.rights[(int)(((java.math.BigInteger)(idx_7)).longValue())])).toArray(java.math.BigInteger[]::new)));
                queue_hd_3 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_hd_3), java.util.stream.Stream.of(hd_3.add(java.math.BigInteger.valueOf(1)))).toArray(java.math.BigInteger[]::new)));
            }
        }
        sort_pairs(((java.math.BigInteger[])(hds_1)), ((java.math.BigInteger[])(vals_3)));
        return vals_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NIL = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
            tree = make_tree();
            System.out.println(java.util.Arrays.toString(right_view(tree)));
            System.out.println(java.util.Arrays.toString(left_view(tree)));
            System.out.println(java.util.Arrays.toString(top_view(tree)));
            System.out.println(java.util.Arrays.toString(bottom_view(tree)));
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
}
