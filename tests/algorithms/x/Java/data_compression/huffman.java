public class Main {
    interface Huffman {}

    static class Leaf implements Huffman {
        String symbol;
        long freq;
        Leaf(String symbol, long freq) {
            this.symbol = symbol;
            this.freq = freq;
        }
        Leaf() {}
        @Override public String toString() {
            return String.format("{'symbol': '%s', 'freq': %s}", String.valueOf(symbol), String.valueOf(freq));
        }
    }

    static class Node implements Huffman {
        long freq;
        Huffman left;
        Huffman right;
        Node(long freq, Huffman left, Huffman right) {
            this.freq = freq;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'freq': %s, 'left': %s, 'right': %s}", String.valueOf(freq), String.valueOf(left), String.valueOf(right));
        }
    }


    static long get_freq(Huffman n) {
        return ((long)(n instanceof Leaf ? ((Leaf)(n)).freq : ((Node)(n)).freq));
    }

    static Huffman[] sort_nodes(Huffman[] nodes) {
        Huffman[] arr = ((Huffman[])(nodes));
        long i = 1;
        while (i < arr.length) {
            Huffman key = arr[(int)(i)];
            long j = i - 1;
            while (j >= 0 && get_freq(arr[(int)(j)]) > get_freq(key)) {
arr[(int)(j + 1)] = arr[(int)(j)];
                j = j - 1;
            }
arr[(int)(j + 1)] = key;
            i = i + 1;
        }
        return arr;
    }

    static Huffman[] rest(Huffman[] nodes) {
        Huffman[] res = ((Huffman[])(new Huffman[]{}));
        long i_1 = 1;
        while (i_1 < nodes.length) {
            res = ((Huffman[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(nodes[(int)(i_1)])).toArray(Huffman[]::new)));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static Huffman[] count_freq(String text) {
        String[] chars = ((String[])(new String[]{}));
        long[] freqs = ((long[])(new long[]{}));
        long i_2 = 0;
        while (i_2 < _runeLen(text)) {
            String c = _substr(text, (int)(i_2), (int)(i_2 + 1));
            long j_1 = 0;
            boolean found = false;
            while (j_1 < chars.length) {
                if ((chars[(int)(j_1)].equals(c))) {
freqs[(int)(j_1)] = freqs[(int)(j_1)] + 1;
                    found = true;
                    break;
                }
                j_1 = j_1 + 1;
            }
            if (!found) {
                chars = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(chars), java.util.stream.Stream.of(c)).toArray(String[]::new)));
                freqs = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(freqs), java.util.stream.LongStream.of(1)).toArray()));
            }
            i_2 = i_2 + 1;
        }
        Huffman[] leaves = ((Huffman[])(new Huffman[]{}));
        long k = 0;
        while (k < chars.length) {
            leaves = java.util.stream.Stream.concat(java.util.Arrays.stream(leaves), java.util.stream.Stream.of(new Leaf(chars[(int)(k)], freqs[(int)(k)]))).toArray(Huffman[]::new);
            k = k + 1;
        }
        return sort_nodes(((Huffman[])(leaves)));
    }

    static Huffman build_tree(Huffman[] nodes) {
        Huffman[] arr_1 = ((Huffman[])(nodes));
        while (arr_1.length > 1) {
            Huffman left = arr_1[(int)(0)];
            arr_1 = ((Huffman[])(rest(((Huffman[])(arr_1)))));
            Huffman right = arr_1[(int)(0)];
            arr_1 = ((Huffman[])(rest(((Huffman[])(arr_1)))));
            Node node = new Node(get_freq(left) + get_freq(right), left, right);
            arr_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(node)).toArray(Huffman[]::new);
            arr_1 = sort_nodes(((Huffman[])(arr_1)));
        }
        return ((Huffman)(arr_1[(int)(0)]));
    }

    static String[][] concat_pairs(String[][] a, String[][] b) {
        String[][] res_1 = ((String[][])(a));
        long i_3 = 0;
        while (i_3 < b.length) {
            res_1 = ((String[][])(appendObj((String[][])res_1, b[(int)(i_3)])));
            i_3 = i_3 + 1;
        }
        return res_1;
    }

    static String[][] collect_codes(Huffman tree, String prefix) {
        return tree instanceof Leaf ? new String[][]{new String[]{((Leaf)(tree)).symbol, prefix}} : concat_pairs(((String[][])(collect_codes(((Node)(tree)).left, prefix + "0"))), ((String[][])(collect_codes(((Node)(tree)).right, prefix + "1"))));
    }

    static String find_code(String[][] pairs, String ch) {
        long i_4 = 0;
        while (i_4 < pairs.length) {
            if ((pairs[(int)(i_4)][(int)(0)].equals(ch))) {
                return pairs[(int)(i_4)][(int)(1)];
            }
            i_4 = i_4 + 1;
        }
        return "";
    }

    static String huffman_encode(String text) {
        if ((text.equals(""))) {
            return "";
        }
        Huffman[] leaves_1 = ((Huffman[])(count_freq(text)));
        Huffman tree = build_tree(((Huffman[])(leaves_1)));
        String[][] codes = ((String[][])(collect_codes(tree, "")));
        String encoded = "";
        long i_5 = 0;
        while (i_5 < _runeLen(text)) {
            String c_1 = _substr(text, (int)(i_5), (int)(i_5 + 1));
            encoded = encoded + String.valueOf(find_code(((String[][])(codes)), c_1)) + " ";
            i_5 = i_5 + 1;
        }
        return encoded;
    }
    public static void main(String[] args) {
        System.out.println(huffman_encode("beep boop beer!"));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
