public class Main {
    static class Node {
        java.math.BigInteger key;
        java.math.BigInteger freq;
        Node(java.math.BigInteger key, java.math.BigInteger freq) {
            this.key = key;
            this.freq = freq;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'key': %s, 'freq': %s}", String.valueOf(key), String.valueOf(freq));
        }
    }


    static Node[] sort_nodes(Node[] nodes) {
        Node[] arr = ((Node[])(nodes));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            Node key_node_1 = arr[_idx((arr).length, ((java.math.BigInteger)(i_1)).longValue())];
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
            while (j_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
                Node temp_1 = arr[_idx((arr).length, ((java.math.BigInteger)(j_1)).longValue())];
                if (temp_1.key.compareTo(key_node_1.key) > 0) {
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = temp_1;
                    j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
                } else {
                    break;
                }
            }
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = key_node_1;
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((Node[])(arr));
    }

    static void print_node(Node n) {
        System.out.println("Node(key=" + _p(n.key) + ", freq=" + _p(n.freq) + ")");
    }

    static void print_binary_search_tree(java.math.BigInteger[][] root, java.math.BigInteger[] keys, java.math.BigInteger i, java.math.BigInteger j, java.math.BigInteger parent, boolean is_left) {
        if (i.compareTo(j) > 0 || i.compareTo(java.math.BigInteger.valueOf(0)) < 0 || j.compareTo(new java.math.BigInteger(String.valueOf(root.length)).subtract(java.math.BigInteger.valueOf(1))) > 0) {
            return;
        }
        java.math.BigInteger node_1 = new java.math.BigInteger(String.valueOf(root[_idx((root).length, ((java.math.BigInteger)(i)).longValue())][_idx((root[_idx((root).length, ((java.math.BigInteger)(i)).longValue())]).length, ((java.math.BigInteger)(j)).longValue())]));
        if (parent.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            System.out.println(_p(_geto(keys, ((Number)(node_1)).intValue())) + " is the root of the binary search tree.");
        } else         if (is_left) {
            System.out.println(_p(_geto(keys, ((Number)(node_1)).intValue())) + " is the left child of key " + _p(parent) + ".");
        } else {
            System.out.println(_p(_geto(keys, ((Number)(node_1)).intValue())) + " is the right child of key " + _p(parent) + ".");
        }
        print_binary_search_tree(((java.math.BigInteger[][])(root)), ((java.math.BigInteger[])(keys)), new java.math.BigInteger(String.valueOf(i)), new java.math.BigInteger(String.valueOf(node_1.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(keys[_idx((keys).length, ((java.math.BigInteger)(node_1)).longValue())])), true);
        print_binary_search_tree(((java.math.BigInteger[][])(root)), ((java.math.BigInteger[])(keys)), new java.math.BigInteger(String.valueOf(node_1.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(j)), new java.math.BigInteger(String.valueOf(keys[_idx((keys).length, ((java.math.BigInteger)(node_1)).longValue())])), false);
    }

    static void find_optimal_binary_search_tree(Node[] original_nodes) {
        Node[] nodes = ((Node[])(sort_nodes(((Node[])(original_nodes)))));
        java.math.BigInteger n_1 = new java.math.BigInteger(String.valueOf(nodes.length));
        java.math.BigInteger[] keys_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] freqs_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) < 0) {
            Node node_3 = nodes[_idx((nodes).length, ((java.math.BigInteger)(i_3)).longValue())];
            keys_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(keys_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(node_3.key)))).toArray(java.math.BigInteger[]::new)));
            freqs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(freqs_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(node_3.freq)))).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[][] dp_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger[][] total_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger[][] root_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) < 0) {
            java.math.BigInteger[] dp_row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger[] total_row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger[] root_row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            while (j_3.compareTo(n_1) < 0) {
                if (i_3.compareTo(j_3) == 0) {
                    dp_row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_row_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(freqs_1[_idx((freqs_1).length, ((java.math.BigInteger)(i_3)).longValue())])))).toArray(java.math.BigInteger[]::new)));
                    total_row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(total_row_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(freqs_1[_idx((freqs_1).length, ((java.math.BigInteger)(i_3)).longValue())])))).toArray(java.math.BigInteger[]::new)));
                    root_row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(root_row_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(i_3)))).toArray(java.math.BigInteger[]::new)));
                } else {
                    dp_row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                    total_row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(total_row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                    root_row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(root_row_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(0))).toArray(java.math.BigInteger[]::new)));
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            dp_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dp_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(dp_row_1))})).toArray(java.math.BigInteger[][]::new)));
            total_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(total_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(total_row_1))})).toArray(java.math.BigInteger[][]::new)));
            root_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(root_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(root_row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger interval_length_1 = java.math.BigInteger.valueOf(2);
        java.math.BigInteger INF_1 = java.math.BigInteger.valueOf(2147483647);
        while (interval_length_1.compareTo(n_1) <= 0) {
            i_3 = java.math.BigInteger.valueOf(0);
            while (i_3.compareTo(n_1.subtract(interval_length_1).add(java.math.BigInteger.valueOf(1))) < 0) {
                java.math.BigInteger j_5 = new java.math.BigInteger(String.valueOf(i_3.add(interval_length_1).subtract(java.math.BigInteger.valueOf(1))));
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_5)).longValue())] = new java.math.BigInteger(String.valueOf(INF_1));
total_1[_idx((total_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_5)).longValue())] = new java.math.BigInteger(String.valueOf(total_1[_idx((total_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((total_1[_idx((total_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_5.subtract(java.math.BigInteger.valueOf(1)))).longValue())].add(freqs_1[_idx((freqs_1).length, ((java.math.BigInteger)(j_5)).longValue())])));
                java.math.BigInteger r_1 = new java.math.BigInteger(String.valueOf(root_1[_idx((root_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((root_1[_idx((root_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_5.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
                while (r_1.compareTo(root_1[_idx((root_1).length, ((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())][_idx((root_1[_idx((root_1).length, ((java.math.BigInteger)(i_3.add(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_5)).longValue())]) <= 0) {
                    java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(r_1.compareTo(i_3) != 0 ? dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(r_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())] : 0));
                    java.math.BigInteger right_1 = new java.math.BigInteger(String.valueOf(r_1.compareTo(j_5) != 0 ? dp_1[_idx((dp_1).length, ((java.math.BigInteger)(r_1.add(java.math.BigInteger.valueOf(1)))).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(r_1.add(java.math.BigInteger.valueOf(1)))).longValue())]).length, ((java.math.BigInteger)(j_5)).longValue())] : 0));
                    java.math.BigInteger cost_1 = new java.math.BigInteger(String.valueOf(left_1.add(total_1[_idx((total_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((total_1[_idx((total_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_5)).longValue())]).add(right_1)));
                    if (dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_5)).longValue())].compareTo(cost_1) > 0) {
dp_1[_idx((dp_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_5)).longValue())] = new java.math.BigInteger(String.valueOf(cost_1));
root_1[_idx((root_1).length, ((java.math.BigInteger)(i_3)).longValue())][(int)(((java.math.BigInteger)(j_5)).longValue())] = new java.math.BigInteger(String.valueOf(r_1));
                    }
                    r_1 = new java.math.BigInteger(String.valueOf(r_1.add(java.math.BigInteger.valueOf(1))));
                }
                i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            }
            interval_length_1 = new java.math.BigInteger(String.valueOf(interval_length_1.add(java.math.BigInteger.valueOf(1))));
        }
        System.out.println("Binary search tree nodes:");
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(n_1) < 0) {
            print_node(nodes[_idx((nodes).length, ((java.math.BigInteger)(i_3)).longValue())]);
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        System.out.println("\nThe cost of optimal BST for given tree nodes is " + _p(_geto(dp_1[_idx((dp_1).length, 0L)], ((Number)(n_1.subtract(java.math.BigInteger.valueOf(1)))).intValue())) + ".");
        print_binary_search_tree(((java.math.BigInteger[][])(root_1)), ((java.math.BigInteger[])(keys_1)), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(n_1.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(((java.math.BigInteger.valueOf(1)).negate()))), false);
    }

    static void main() {
        Node[] nodes_1 = ((Node[])(new Node[]{new Node(java.math.BigInteger.valueOf(12), java.math.BigInteger.valueOf(8)), new Node(java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(34)), new Node(java.math.BigInteger.valueOf(20), java.math.BigInteger.valueOf(50)), new Node(java.math.BigInteger.valueOf(42), java.math.BigInteger.valueOf(3)), new Node(java.math.BigInteger.valueOf(25), java.math.BigInteger.valueOf(40)), new Node(java.math.BigInteger.valueOf(37), java.math.BigInteger.valueOf(30))}));
        find_optimal_binary_search_tree(((Node[])(nodes_1)));
    }
    public static void main(String[] args) {
        main();
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
