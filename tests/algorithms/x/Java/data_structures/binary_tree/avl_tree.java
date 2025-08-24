public class Main {
    static java.math.BigInteger NIL;
    static java.util.Map<String,java.math.BigInteger>[] nodes = ((java.util.Map<String,java.math.BigInteger>[])((java.util.Map<String,java.math.BigInteger>[])new java.util.Map[]{}));

    static java.math.BigInteger new_node(java.math.BigInteger value) {
        java.util.Map<String,java.math.BigInteger> node = ((java.util.Map<String,java.math.BigInteger>)(new java.util.LinkedHashMap<String, java.math.BigInteger>(java.util.Map.ofEntries(java.util.Map.entry("data", new java.math.BigInteger(String.valueOf(value))), java.util.Map.entry("left", new java.math.BigInteger(String.valueOf(NIL))), java.util.Map.entry("right", new java.math.BigInteger(String.valueOf(NIL))), java.util.Map.entry("height", java.math.BigInteger.valueOf(1))))));
        nodes = ((java.util.Map<String,java.math.BigInteger>[])(appendObj((java.util.Map<String,java.math.BigInteger>[])nodes, node)));
        return new java.math.BigInteger(String.valueOf(nodes.length)).subtract(java.math.BigInteger.valueOf(1));
    }

    static java.math.BigInteger get_height(java.math.BigInteger i) {
        if (i.compareTo(NIL) == 0) {
            return 0;
        }
        return ((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("height"));
    }

    static java.math.BigInteger my_max(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) > 0) {
            return a;
        }
        return b;
    }

    static void update_height(java.math.BigInteger i) {
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("height"))] = my_max(new java.math.BigInteger(String.valueOf(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left"))))))), new java.math.BigInteger(String.valueOf(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))))))).add(java.math.BigInteger.valueOf(1));
    }

    static java.math.BigInteger right_rotation(java.math.BigInteger i) {
        java.math.BigInteger left = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left"))));
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("left"))] = ((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(left)).longValue())])).get("right"));
nodes[(int)(((java.math.BigInteger)(left)).longValue())][(int)((long)("right"))] = i;
        update_height(new java.math.BigInteger(String.valueOf(i)));
        update_height(new java.math.BigInteger(String.valueOf(left)));
        return left;
    }

    static java.math.BigInteger left_rotation(java.math.BigInteger i) {
        java.math.BigInteger right = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right"))));
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("right"))] = ((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(right)).longValue())])).get("left"));
nodes[(int)(((java.math.BigInteger)(right)).longValue())][(int)((long)("left"))] = i;
        update_height(new java.math.BigInteger(String.valueOf(i)));
        update_height(new java.math.BigInteger(String.valueOf(right)));
        return right;
    }

    static java.math.BigInteger lr_rotation(java.math.BigInteger i) {
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("left"))] = left_rotation(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))));
        return right_rotation(new java.math.BigInteger(String.valueOf(i)));
    }

    static java.math.BigInteger rl_rotation(java.math.BigInteger i) {
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("right"))] = right_rotation(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))));
        return left_rotation(new java.math.BigInteger(String.valueOf(i)));
    }

    static java.math.BigInteger insert_node(java.math.BigInteger i, java.math.BigInteger value) {
        if (i.compareTo(NIL) == 0) {
            return new_node(new java.math.BigInteger(String.valueOf(value)));
        }
        if (value.compareTo(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("data"))) < 0) {
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("left"))] = insert_node(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))), new java.math.BigInteger(String.valueOf(value)));
            if (get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left"))))).subtract(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))))).compareTo(java.math.BigInteger.valueOf(2)) == 0) {
                if (value.compareTo(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))).longValue())])).get("data"))) < 0) {
                    i = new java.math.BigInteger(String.valueOf(right_rotation(new java.math.BigInteger(String.valueOf(i)))));
                } else {
                    i = new java.math.BigInteger(String.valueOf(lr_rotation(new java.math.BigInteger(String.valueOf(i)))));
                }
            }
        } else {
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("right"))] = insert_node(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))), new java.math.BigInteger(String.valueOf(value)));
            if (get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right"))))).subtract(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))))).compareTo(java.math.BigInteger.valueOf(2)) == 0) {
                if (value.compareTo(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))).longValue())])).get("data"))) < 0) {
                    i = new java.math.BigInteger(String.valueOf(rl_rotation(new java.math.BigInteger(String.valueOf(i)))));
                } else {
                    i = new java.math.BigInteger(String.valueOf(left_rotation(new java.math.BigInteger(String.valueOf(i)))));
                }
            }
        }
        update_height(new java.math.BigInteger(String.valueOf(i)));
        return i;
    }

    static java.math.BigInteger get_left_most(java.math.BigInteger i) {
        java.math.BigInteger cur = new java.math.BigInteger(String.valueOf(i));
        while (((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(cur)).longValue())])).get("left")).compareTo(NIL) != 0) {
            cur = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(cur)).longValue())])).get("left"))));
        }
        return ((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(cur)).longValue())])).get("data"));
    }

    static java.math.BigInteger del_node(java.math.BigInteger i, java.math.BigInteger value) {
        if (i.compareTo(NIL) == 0) {
            return NIL;
        }
        if (value.compareTo(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("data"))) < 0) {
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("left"))] = del_node(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))), new java.math.BigInteger(String.valueOf(value)));
        } else         if (value.compareTo(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("data"))) > 0) {
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("right"))] = del_node(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))), new java.math.BigInteger(String.valueOf(value)));
        } else         if (((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")).compareTo(NIL) != 0 && ((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")).compareTo(NIL) != 0) {
            java.math.BigInteger temp_1 = new java.math.BigInteger(String.valueOf(get_left_most(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))))));
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("data"))] = temp_1;
nodes[(int)(((java.math.BigInteger)(i)).longValue())][(int)((long)("right"))] = del_node(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))), new java.math.BigInteger(String.valueOf(temp_1)));
        } else         if (((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")).compareTo(NIL) != 0) {
            i = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left"))));
        } else {
            i = new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right"))));
        }
        if (i.compareTo(NIL) == 0) {
            return NIL;
        }
        java.math.BigInteger lh_1 = new java.math.BigInteger(String.valueOf(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))))));
        java.math.BigInteger rh_1 = new java.math.BigInteger(String.valueOf(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))))));
        if (rh_1.subtract(lh_1).compareTo(java.math.BigInteger.valueOf(2)) == 0) {
            if (get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))).longValue())])).get("right"))))).compareTo(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right")))).longValue())])).get("left")))))) > 0) {
                i = new java.math.BigInteger(String.valueOf(left_rotation(new java.math.BigInteger(String.valueOf(i)))));
            } else {
                i = new java.math.BigInteger(String.valueOf(rl_rotation(new java.math.BigInteger(String.valueOf(i)))));
            }
        } else         if (lh_1.subtract(rh_1).compareTo(java.math.BigInteger.valueOf(2)) == 0) {
            if (get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))).longValue())])).get("left"))))).compareTo(get_height(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left")))).longValue())])).get("right")))))) > 0) {
                i = new java.math.BigInteger(String.valueOf(right_rotation(new java.math.BigInteger(String.valueOf(i)))));
            } else {
                i = new java.math.BigInteger(String.valueOf(lr_rotation(new java.math.BigInteger(String.valueOf(i)))));
            }
        }
        update_height(new java.math.BigInteger(String.valueOf(i)));
        return i;
    }

    static String inorder(java.math.BigInteger i) {
        if (i.compareTo(NIL) == 0) {
            return "";
        }
        String left_2 = String.valueOf(inorder(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("left"))))));
        String right_2 = String.valueOf(inorder(new java.math.BigInteger(String.valueOf(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("right"))))));
        String res_1 = _p(((java.math.BigInteger)(((java.util.Map)nodes[(int)(((java.math.BigInteger)(i)).longValue())])).get("data")));
        if (!(left_2.equals(""))) {
            res_1 = left_2 + " " + res_1;
        }
        if (!(right_2.equals(""))) {
            res_1 = res_1 + " " + right_2;
        }
        return res_1;
    }

    static void main() {
        nodes = ((java.util.Map<String,java.math.BigInteger>[])((java.util.Map<String,java.math.BigInteger>[])new java.util.Map[]{}));
        java.math.BigInteger root_1 = new java.math.BigInteger(String.valueOf(NIL));
        root_1 = new java.math.BigInteger(String.valueOf(insert_node(new java.math.BigInteger(String.valueOf(root_1)), java.math.BigInteger.valueOf(4))));
        root_1 = new java.math.BigInteger(String.valueOf(insert_node(new java.math.BigInteger(String.valueOf(root_1)), java.math.BigInteger.valueOf(2))));
        root_1 = new java.math.BigInteger(String.valueOf(insert_node(new java.math.BigInteger(String.valueOf(root_1)), java.math.BigInteger.valueOf(3))));
        System.out.println(inorder(new java.math.BigInteger(String.valueOf(root_1))));
        System.out.println(_p(get_height(new java.math.BigInteger(String.valueOf(root_1)))));
        root_1 = new java.math.BigInteger(String.valueOf(del_node(new java.math.BigInteger(String.valueOf(root_1)), java.math.BigInteger.valueOf(3))));
        System.out.println(inorder(new java.math.BigInteger(String.valueOf(root_1))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NIL = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
