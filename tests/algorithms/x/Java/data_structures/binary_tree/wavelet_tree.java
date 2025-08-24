public class Main {
    static class Node {
        java.math.BigInteger minn;
        java.math.BigInteger maxx;
        java.math.BigInteger[] map_left;
        java.math.BigInteger left;
        java.math.BigInteger right;
        Node(java.math.BigInteger minn, java.math.BigInteger maxx, java.math.BigInteger[] map_left, java.math.BigInteger left, java.math.BigInteger right) {
            this.minn = minn;
            this.maxx = maxx;
            this.map_left = map_left;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'minn': %s, 'maxx': %s, 'map_left': %s, 'left': %s, 'right': %s}", String.valueOf(minn), String.valueOf(maxx), String.valueOf(map_left), String.valueOf(left), String.valueOf(right));
        }
    }

    static Node[] nodes = ((Node[])(new Node[]{}));
    static java.math.BigInteger[] test_array = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(7)}));
    static java.math.BigInteger root;

    static java.math.BigInteger[] make_list(java.math.BigInteger length, java.math.BigInteger value) {
        java.math.BigInteger[] lst = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(length) < 0) {
            lst = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lst), java.util.stream.Stream.of(value)).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return lst;
    }

    static java.math.BigInteger min_list(java.math.BigInteger[] arr) {
        java.math.BigInteger m = new java.math.BigInteger(String.valueOf(arr[(int)(0L)]));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            if (arr[(int)(((java.math.BigInteger)(i_3)).longValue())].compareTo(m) < 0) {
                m = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(i_3)).longValue())]));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return m;
    }

    static java.math.BigInteger max_list(java.math.BigInteger[] arr) {
        java.math.BigInteger m_1 = new java.math.BigInteger(String.valueOf(arr[(int)(0L)]));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(1);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            if (arr[(int)(((java.math.BigInteger)(i_5)).longValue())].compareTo(m_1) > 0) {
                m_1 = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(i_5)).longValue())]));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        return m_1;
    }

    static java.math.BigInteger build_tree(java.math.BigInteger[] arr) {
        Node n = new Node(min_list(((java.math.BigInteger[])(arr))), max_list(((java.math.BigInteger[])(arr))), make_list(new java.math.BigInteger(String.valueOf(arr.length)), java.math.BigInteger.valueOf(0)), (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate());
        if (n.minn.compareTo(n.maxx) == 0) {
            nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(n)).toArray(Node[]::new)));
            return new java.math.BigInteger(String.valueOf(nodes.length)).subtract(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger pivot_1 = new java.math.BigInteger(String.valueOf((n.minn.add(n.maxx)).divide(java.math.BigInteger.valueOf(2))));
        java.math.BigInteger[] left_arr_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger[] right_arr_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            java.math.BigInteger num_1 = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(i_7)).longValue())]));
            if (num_1.compareTo(pivot_1) <= 0) {
                left_arr_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(left_arr_1), java.util.stream.Stream.of(num_1)).toArray(java.math.BigInteger[]::new)));
            } else {
                right_arr_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(right_arr_1), java.util.stream.Stream.of(num_1)).toArray(java.math.BigInteger[]::new)));
            }
            java.math.BigInteger[] ml_1 = ((java.math.BigInteger[])(n.map_left));
ml_1[(int)(((java.math.BigInteger)(i_7)).longValue())] = new java.math.BigInteger(String.valueOf(left_arr_1.length));
n.map_left = ml_1;
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        if (new java.math.BigInteger(String.valueOf(left_arr_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
n.left = build_tree(((java.math.BigInteger[])(left_arr_1)));
        }
        if (new java.math.BigInteger(String.valueOf(right_arr_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
n.right = build_tree(((java.math.BigInteger[])(right_arr_1)));
        }
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(n)).toArray(Node[]::new)));
        return new java.math.BigInteger(String.valueOf(nodes.length)).subtract(java.math.BigInteger.valueOf(1));
    }

    static java.math.BigInteger rank_till_index(java.math.BigInteger node_idx, java.math.BigInteger num, java.math.BigInteger index) {
        if (index.compareTo(java.math.BigInteger.valueOf(0)) < 0 || node_idx.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            return 0;
        }
        Node node_1 = nodes[(int)(((java.math.BigInteger)(node_idx)).longValue())];
        if (node_1.minn.compareTo(node_1.maxx) == 0) {
            if (node_1.minn.compareTo(num) == 0) {
                return index.add(java.math.BigInteger.valueOf(1));
            } else {
                return 0;
            }
        }
        java.math.BigInteger pivot_3 = new java.math.BigInteger(String.valueOf((node_1.minn.add(node_1.maxx)).divide(java.math.BigInteger.valueOf(2))));
        if (num.compareTo(pivot_3) <= 0) {
            return rank_till_index(new java.math.BigInteger(String.valueOf(node_1.left)), new java.math.BigInteger(String.valueOf(num)), new java.math.BigInteger(String.valueOf(node_1.map_left[(int)(((java.math.BigInteger)(index)).longValue())].subtract(java.math.BigInteger.valueOf(1)))));
        } else {
            return rank_till_index(new java.math.BigInteger(String.valueOf(node_1.right)), new java.math.BigInteger(String.valueOf(num)), new java.math.BigInteger(String.valueOf(index.subtract(node_1.map_left[(int)(((java.math.BigInteger)(index)).longValue())]))));
        }
    }

    static java.math.BigInteger rank(java.math.BigInteger node_idx, java.math.BigInteger num, java.math.BigInteger start, java.math.BigInteger end) {
        if (start.compareTo(end) > 0) {
            return 0;
        }
        java.math.BigInteger rank_till_end_1 = new java.math.BigInteger(String.valueOf(rank_till_index(new java.math.BigInteger(String.valueOf(node_idx)), new java.math.BigInteger(String.valueOf(num)), new java.math.BigInteger(String.valueOf(end)))));
        java.math.BigInteger rank_before_start_1 = new java.math.BigInteger(String.valueOf(rank_till_index(new java.math.BigInteger(String.valueOf(node_idx)), new java.math.BigInteger(String.valueOf(num)), new java.math.BigInteger(String.valueOf(start.subtract(java.math.BigInteger.valueOf(1)))))));
        return rank_till_end_1.subtract(rank_before_start_1);
    }

    static java.math.BigInteger quantile(java.math.BigInteger node_idx, java.math.BigInteger index, java.math.BigInteger start, java.math.BigInteger end) {
        if (index.compareTo((end.subtract(start))) > 0 || start.compareTo(end) > 0 || node_idx.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            return (java.math.BigInteger.valueOf(1)).negate();
        }
        Node node_3 = nodes[(int)(((java.math.BigInteger)(node_idx)).longValue())];
        if (node_3.minn.compareTo(node_3.maxx) == 0) {
            return node_3.minn;
        }
        java.math.BigInteger left_start_1 = new java.math.BigInteger(String.valueOf(start.compareTo(java.math.BigInteger.valueOf(0)) == 0 ? 0 : node_3.map_left[(int)(((java.math.BigInteger)(start.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
        java.math.BigInteger num_left_1 = new java.math.BigInteger(String.valueOf(node_3.map_left[(int)(((java.math.BigInteger)(end)).longValue())].subtract(left_start_1)));
        if (num_left_1.compareTo(index) > 0) {
            return quantile(new java.math.BigInteger(String.valueOf(node_3.left)), new java.math.BigInteger(String.valueOf(index)), new java.math.BigInteger(String.valueOf(left_start_1)), new java.math.BigInteger(String.valueOf(node_3.map_left[(int)(((java.math.BigInteger)(end)).longValue())].subtract(java.math.BigInteger.valueOf(1)))));
        } else {
            return quantile(new java.math.BigInteger(String.valueOf(node_3.right)), new java.math.BigInteger(String.valueOf(index.subtract(num_left_1))), new java.math.BigInteger(String.valueOf(start.subtract(left_start_1))), new java.math.BigInteger(String.valueOf(end.subtract(node_3.map_left[(int)(((java.math.BigInteger)(end)).longValue())]))));
        }
    }

    static java.math.BigInteger range_counting(java.math.BigInteger node_idx, java.math.BigInteger start, java.math.BigInteger end, java.math.BigInteger start_num, java.math.BigInteger end_num) {
        if (start.compareTo(end) > 0 || node_idx.compareTo(java.math.BigInteger.valueOf(0)) < 0 || start_num.compareTo(end_num) > 0) {
            return 0;
        }
        Node node_5 = nodes[(int)(((java.math.BigInteger)(node_idx)).longValue())];
        if (node_5.minn.compareTo(end_num) > 0 || node_5.maxx.compareTo(start_num) < 0) {
            return 0;
        }
        if (start_num.compareTo(node_5.minn) <= 0 && node_5.maxx.compareTo(end_num) <= 0) {
            return end.subtract(start).add(java.math.BigInteger.valueOf(1));
        }
        java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(range_counting(new java.math.BigInteger(String.valueOf(node_5.left)), new java.math.BigInteger(String.valueOf(start.compareTo(java.math.BigInteger.valueOf(0)) == 0 ? 0 : node_5.map_left[(int)(((java.math.BigInteger)(start.subtract(java.math.BigInteger.valueOf(1)))).longValue())])), new java.math.BigInteger(String.valueOf(node_5.map_left[(int)(((java.math.BigInteger)(end)).longValue())].subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(start_num)), new java.math.BigInteger(String.valueOf(end_num)))));
        java.math.BigInteger right_1 = new java.math.BigInteger(String.valueOf(range_counting(new java.math.BigInteger(String.valueOf(node_5.right)), new java.math.BigInteger(String.valueOf(start.subtract((start.compareTo(java.math.BigInteger.valueOf(0)) == 0 ? 0 : node_5.map_left[(int)(((java.math.BigInteger)(start.subtract(java.math.BigInteger.valueOf(1)))).longValue())])))), new java.math.BigInteger(String.valueOf(end.subtract(node_5.map_left[(int)(((java.math.BigInteger)(end)).longValue())]))), new java.math.BigInteger(String.valueOf(start_num)), new java.math.BigInteger(String.valueOf(end_num)))));
        return left_1.add(right_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            root = new java.math.BigInteger(String.valueOf(build_tree(((java.math.BigInteger[])(test_array)))));
            System.out.println("rank_till_index 6 at 6 -> " + _p(rank_till_index(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(6))));
            System.out.println("rank 6 in [3,13] -> " + _p(rank(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(13))));
            System.out.println("quantile index 2 in [2,5] -> " + _p(quantile(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5))));
            System.out.println("range_counting [3,7] in [1,10] -> " + _p(range_counting(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(7))));
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
