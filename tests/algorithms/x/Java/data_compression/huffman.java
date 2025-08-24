public class Main {
    interface Huffman {}

    static class Leaf implements Huffman {
        String symbol;
        java.math.BigInteger freq;
        Leaf(String symbol, java.math.BigInteger freq) {
            this.symbol = symbol;
            this.freq = freq;
        }
        Leaf() {}
        @Override public String toString() {
            return String.format("{'symbol': '%s', 'freq': %s}", String.valueOf(symbol), String.valueOf(freq));
        }
    }

    static class Node implements Huffman {
        java.math.BigInteger freq;
        Huffman left;
        Huffman right;
        Node(java.math.BigInteger freq, Huffman left, Huffman right) {
            this.freq = freq;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'freq': %s, 'left': %s, 'right': %s}", String.valueOf(freq), String.valueOf(left), String.valueOf(right));
        }
    }


    static java.math.BigInteger get_freq(Huffman n) {
        return new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(n instanceof Leaf ? ((Leaf)(n)).freq : ((Node)(n)).freq))));
    }

    static Huffman[] sort_nodes(Huffman[] nodes) {
        Huffman[] arr = ((Huffman[])(nodes));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(arr.length))) < 0) {
            Huffman key_1 = arr[(int)(((java.math.BigInteger)(i_1)).longValue())];
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
            while (j_1.compareTo(java.math.BigInteger.valueOf(0)) >= 0 && get_freq(arr[(int)(((java.math.BigInteger)(j_1)).longValue())]).compareTo(get_freq(key_1)) > 0) {
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = arr[(int)(((java.math.BigInteger)(j_1)).longValue())];
                j_1 = new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1))));
            }
arr[(int)(((java.math.BigInteger)(j_1.add(java.math.BigInteger.valueOf(1)))).longValue())] = key_1;
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((Huffman[])(arr));
    }

    static Huffman[] rest(Huffman[] nodes) {
        Huffman[] res = ((Huffman[])(new Huffman[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(nodes.length))) < 0) {
            res = ((Huffman[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(nodes[(int)(((java.math.BigInteger)(i_3)).longValue())])).toArray(Huffman[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((Huffman[])(res));
    }

    static Huffman[] count_freq(String text) {
        String[] chars = ((String[])(new String[]{}));
        java.math.BigInteger[] freqs_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(text)))) < 0) {
            String c_1 = _substr(text, (int)(((java.math.BigInteger)(i_5)).longValue()), (int)(((java.math.BigInteger)(i_5.add(java.math.BigInteger.valueOf(1)))).longValue()));
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            boolean found_1 = false;
            while (j_3.compareTo(new java.math.BigInteger(String.valueOf(chars.length))) < 0) {
                if ((chars[(int)(((java.math.BigInteger)(j_3)).longValue())].equals(c_1))) {
freqs_1[(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(freqs_1[(int)(((java.math.BigInteger)(j_3)).longValue())].add(java.math.BigInteger.valueOf(1))));
                    found_1 = true;
                    break;
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            if (!found_1) {
                chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(c_1)).toArray(String[]::new)));
                freqs_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(freqs_1), java.util.stream.Stream.of(java.math.BigInteger.valueOf(1))).toArray(java.math.BigInteger[]::new)));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        Huffman[] leaves_1 = ((Huffman[])(new Huffman[]{}));
        java.math.BigInteger k_1 = java.math.BigInteger.valueOf(0);
        while (k_1.compareTo(new java.math.BigInteger(String.valueOf(chars.length))) < 0) {
            leaves_1 = ((Huffman[])(java.util.stream.Stream.concat(java.util.Arrays.stream(leaves_1), java.util.stream.Stream.of(new Leaf(chars[(int)(((java.math.BigInteger)(k_1)).longValue())], freqs_1[(int)(((java.math.BigInteger)(k_1)).longValue())]))).toArray(Huffman[]::new)));
            k_1 = new java.math.BigInteger(String.valueOf(k_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((Huffman[])(sort_nodes(((Huffman[])(leaves_1)))));
    }

    static Huffman build_tree(Huffman[] nodes) {
        Huffman[] arr_1 = ((Huffman[])(nodes));
        while (new java.math.BigInteger(String.valueOf(arr_1.length)).compareTo(java.math.BigInteger.valueOf(1)) > 0) {
            Huffman left_1 = arr_1[(int)(0L)];
            arr_1 = ((Huffman[])(rest(((Huffman[])(arr_1)))));
            Huffman right_1 = arr_1[(int)(0L)];
            arr_1 = ((Huffman[])(rest(((Huffman[])(arr_1)))));
            Node node_1 = new Node(get_freq(left_1).add(get_freq(right_1)), left_1, right_1);
            arr_1 = ((Huffman[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(node_1)).toArray(Huffman[]::new)));
            arr_1 = ((Huffman[])(sort_nodes(((Huffman[])(arr_1)))));
        }
        return arr_1[(int)(0L)];
    }

    static String[][] concat_pairs(String[][] a, String[][] b) {
        String[][] res_1 = ((String[][])(a));
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(new java.math.BigInteger(String.valueOf(b.length))) < 0) {
            res_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new String[][]{String.valueOf(b[(int)(((java.math.BigInteger)(i_7)).longValue())])})).toArray(String[][]::new)));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        return ((String[][])(res_1));
    }

    static String[][] collect_codes(Huffman tree, String prefix) {
        return ((String[][])(tree instanceof Leaf ? new String[][]{((String[])(new String[]{(String)(((Leaf)(tree)).symbol), prefix}))} : concat_pairs(((String[][])(collect_codes(((Node)(tree)).left, prefix + "0"))), ((String[][])(collect_codes(((Node)(tree)).right, prefix + "1"))))));
    }

    static String find_code(String[][] pairs, String ch) {
        java.math.BigInteger i_8 = java.math.BigInteger.valueOf(0);
        while (i_8.compareTo(new java.math.BigInteger(String.valueOf(pairs.length))) < 0) {
            if ((pairs[(int)(((java.math.BigInteger)(i_8)).longValue())][(int)(0L)].equals(ch))) {
                return pairs[(int)(((java.math.BigInteger)(i_8)).longValue())][(int)(1L)];
            }
            i_8 = new java.math.BigInteger(String.valueOf(i_8.add(java.math.BigInteger.valueOf(1))));
        }
        return "";
    }

    static String huffman_encode(String text) {
        if ((text.equals(""))) {
            return "";
        }
        Huffman[] leaves_3 = ((Huffman[])(count_freq(text)));
        Huffman tree_1 = build_tree(((Huffman[])(leaves_3)));
        String[][] codes_1 = ((String[][])(collect_codes(tree_1, "")));
        String encoded_1 = "";
        java.math.BigInteger i_10 = java.math.BigInteger.valueOf(0);
        while (i_10.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(text)))) < 0) {
            String c_3 = _substr(text, (int)(((java.math.BigInteger)(i_10)).longValue()), (int)(((java.math.BigInteger)(i_10.add(java.math.BigInteger.valueOf(1)))).longValue()));
            encoded_1 = encoded_1 + String.valueOf(find_code(((String[][])(codes_1)), c_3)) + " ";
            i_10 = new java.math.BigInteger(String.valueOf(i_10.add(java.math.BigInteger.valueOf(1))));
        }
        return encoded_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(huffman_encode("beep boop beer!"));
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
}
