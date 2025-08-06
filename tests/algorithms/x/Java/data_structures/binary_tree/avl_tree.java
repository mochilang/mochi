public class Main {
    static int NIL;
    static java.util.Map<String,Integer>[] nodes = new java.util.Map<String,Integer>[0];

    static int new_node(int value) {
        java.util.Map<String,Integer> node = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("data", value), java.util.Map.entry("left", NIL), java.util.Map.entry("right", NIL), java.util.Map.entry("height", 1)))));
        nodes = ((java.util.Map<String,Integer>[])(appendObj(nodes, node)));
        return nodes.length - 1;
    }

    static int get_height(int i) {
        if (i == NIL) {
            return 0;
        }
        return ((int)(((java.util.Map)nodes[i])).getOrDefault("height", 0));
    }

    static int my_max(int a, int b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static void update_height(int i) {
nodes[i]["height"] = my_max(get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0)))), get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))))) + 1;
    }

    static int right_rotation(int i) {
        int left = (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0)));
nodes[i]["left"] = ((int)(((java.util.Map)nodes[left])).getOrDefault("right", 0));
nodes[left]["right"] = i;
        update_height(i);
        update_height(left);
        return left;
    }

    static int left_rotation(int i) {
        int right = (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0)));
nodes[i]["right"] = ((int)(((java.util.Map)nodes[right])).getOrDefault("left", 0));
nodes[right]["left"] = i;
        update_height(i);
        update_height(right);
        return right;
    }

    static int lr_rotation(int i) {
nodes[i]["left"] = left_rotation((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))));
        return right_rotation(i);
    }

    static int rl_rotation(int i) {
nodes[i]["right"] = right_rotation((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))));
        return left_rotation(i);
    }

    static int insert_node(int i, int value) {
        if (i == NIL) {
            return new_node(value);
        }
        if (value < (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("data", 0)))) {
nodes[i]["left"] = insert_node((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))), value);
            if (get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0)))) - get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0)))) == 2) {
                if (value < (int)(((int)(((java.util.Map)nodes[((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))])).getOrDefault("data", 0)))) {
                    i = right_rotation(i);
                } else {
                    i = lr_rotation(i);
                }
            }
        } else {
nodes[i]["right"] = insert_node((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))), value);
            if (get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0)))) - get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0)))) == 2) {
                if (value < (int)(((int)(((java.util.Map)nodes[((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))])).getOrDefault("data", 0)))) {
                    i = rl_rotation(i);
                } else {
                    i = left_rotation(i);
                }
            }
        }
        update_height(i);
        return i;
    }

    static int get_left_most(int i) {
        int cur = i;
        while ((int)(((int)(((java.util.Map)nodes[cur])).getOrDefault("left", 0))) != NIL) {
            cur = (int)(((int)(((java.util.Map)nodes[cur])).getOrDefault("left", 0)));
        }
        return ((int)(((java.util.Map)nodes[cur])).getOrDefault("data", 0));
    }

    static int del_node(int i, int value) {
        if (i == NIL) {
            return NIL;
        }
        if (value < (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("data", 0)))) {
nodes[i]["left"] = del_node((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))), value);
        } else         if (value > (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("data", 0)))) {
nodes[i]["right"] = del_node((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))), value);
        } else         if ((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))) != NIL && (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))) != NIL) {
            int temp = get_left_most((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))));
nodes[i]["data"] = temp;
nodes[i]["right"] = del_node((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))), temp);
        } else         if ((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))) != NIL) {
            i = (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0)));
        } else {
            i = (int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0)));
        }
        if (i == NIL) {
            return NIL;
        }
        int lh = get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))));
        int rh = get_height((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))));
        if (rh - lh == 2) {
            if (get_height((int)(((int)(((java.util.Map)nodes[((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))])).getOrDefault("right", 0)))) > get_height((int)(((int)(((java.util.Map)nodes[((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0))])).getOrDefault("left", 0))))) {
                i = left_rotation(i);
            } else {
                i = rl_rotation(i);
            }
        } else         if (lh - rh == 2) {
            if (get_height((int)(((int)(((java.util.Map)nodes[((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))])).getOrDefault("left", 0)))) > get_height((int)(((int)(((java.util.Map)nodes[((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0))])).getOrDefault("right", 0))))) {
                i = right_rotation(i);
            } else {
                i = lr_rotation(i);
            }
        }
        update_height(i);
        return i;
    }

    static String inorder(int i) {
        if (i == NIL) {
            return "";
        }
        String left_1 = String.valueOf(inorder((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("left", 0)))));
        String right_1 = String.valueOf(inorder((int)(((int)(((java.util.Map)nodes[i])).getOrDefault("right", 0)))));
        String res = _p(((int)(((java.util.Map)nodes[i])).getOrDefault("data", 0)));
        if (!(left_1.equals(""))) {
            res = left_1 + " " + res;
        }
        if (!(right_1.equals(""))) {
            res = res + " " + right_1;
        }
        return res;
    }

    static void main() {
        nodes = ((java.util.Map<String,Integer>[])((java.util.Map<String,Integer>[])new java.util.Map[]{}));
        int root = NIL;
        root = insert_node(root, 4);
        root = insert_node(root, 2);
        root = insert_node(root, 3);
        System.out.println(inorder(root));
        System.out.println(_p(get_height(root)));
        root = del_node(root, 3);
        System.out.println(inorder(root));
    }
    public static void main(String[] args) {
        NIL = 0 - 1;
        nodes = ((java.util.Map<String,Integer>[])((java.util.Map<String,Integer>[])new java.util.Map[]{}));
        main();
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
        return String.valueOf(v);
    }
}
