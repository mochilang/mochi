public class Main {

    static String join(String[] xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static java.util.Map<String,Object>[] sortPairs(java.util.Map<String,Object>[] xs) {
        java.util.Map<String,Object>[] arr = ((java.util.Map<String,Object>[])(xs));
        int i_1 = 1;
        while (i_1 < arr.length) {
            int j = i_1;
            while (j > 0 && (((int)(((java.util.Map)arr[j - 1])).getOrDefault("count", 0))) < (((int)(((java.util.Map)arr[j])).getOrDefault("count", 0)))) {
                java.util.Map<String,Object> tmp = arr[j - 1];
arr[j - 1] = arr[j];
arr[j] = tmp;
                j = j - 1;
            }
            i_1 = i_1 + 1;
        }
        return arr;
    }

    static boolean isAlphaNumDot(String ch) {
        return ((ch.compareTo("A") >= 0) && (ch.compareTo("Z") <= 0)) || ((ch.compareTo("a") >= 0) && (ch.compareTo("z") <= 0)) || ((ch.compareTo("0") >= 0) && (ch.compareTo("9") <= 0)) || (ch.equals("_")) || (ch.equals("."));
    }

    static void main() {
        String[] srcLines = ((String[])(new String[]{"package main", "", "import (", "    \"fmt\"", "    \"go/ast\"", "    \"go/parser\"", "    \"go/token\"", "    \"io/ioutil\"", "    \"os\"", "    \"sort\"", ")", "", "func main() {", "    if len(os.Args) != 2 {", "        fmt.Println(\"usage ff <go source filename>\")", "        return", "    }", "    src, err := ioutil.ReadFile(os.Args[1])", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    fs := token.NewFileSet()", "    a, err := parser.ParseFile(fs, os.Args[1], src, 0)", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    f := fs.File(a.Pos())", "    m := make(map[string]int)", "    ast.Inspect(a, func(n ast.Node) bool {", "        if ce, ok := n.(*ast.CallExpr); ok {", "            start := f.Offset(ce.Pos())", "            end := f.Offset(ce.Lparen)", "            m[string(src[start:end])]++", "        }", "        return true", "    })", "    cs := make(calls, 0, len(m))", "    for k, v := range m {", "        cs = append(cs, &call{k, v})", "    }", "    sort.Sort(cs)", "    for i, c := range cs {", "        fmt.Printf(\"%-20s %4d\\n\", c.expr, c.count)", "        if i == 9 {", "            break", "        }", "    }", "}", "", "type call struct {", "    expr  string", "    count int", "}", "type calls []*call", "", "func (c calls) Len() int           { return len(c) }", "func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }", "func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }"}));
        String src = String.valueOf(join(((String[])(srcLines)), "\n"));
        java.util.Map<String,Integer> freq = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i_2 = 0;
        String[] order = ((String[])(new String[]{}));
        while (i_2 < _runeLen(src)) {
            String ch = _substr(src, i_2, i_2 + 1);
            if (((ch.compareTo("A") >= 0) && (ch.compareTo("Z") <= 0)) || ((ch.compareTo("a") >= 0) && (ch.compareTo("z") <= 0)) || (ch.equals("_"))) {
                int j_1 = i_2 + 1;
                while (j_1 < _runeLen(src) && ((Boolean)(isAlphaNumDot(_substr(src, j_1, j_1 + 1))))) {
                    j_1 = j_1 + 1;
                }
                String token = _substr(src, i_2, j_1);
                int k = j_1;
                while (k < _runeLen(src)) {
                    String cc = _substr(src, k, k + 1);
                    if ((cc.equals(" ")) || (cc.equals("\t")) || (cc.equals("\n")) || (cc.equals("\r"))) {
                        k = k + 1;
                    } else {
                        break;
                    }
                }
                if (k < _runeLen(src) && (_substr(src, k, k + 1).equals("("))) {
                    int p = i_2 - 1;
                    while (p >= 0 && ((_substr(src, p, p + 1).equals(" ")) || (_substr(src, p, p + 1).equals("\t")))) {
                        p = p - 1;
                    }
                    boolean skip = false;
                    if (p >= 3) {
                        String before = _substr(src, p - 3, p + 1);
                        if ((before.equals("func"))) {
                            skip = true;
                        }
                    }
                    if (!skip) {
                        if (((Boolean)(freq.containsKey(token)))) {
freq.put(token, (int)(((int)(freq).getOrDefault(token, 0))) + 1);
                        } else {
freq.put(token, 1);
                            order = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(order), java.util.stream.Stream.of(token)).toArray(String[]::new)));
                        }
                    }
                }
                i_2 = j_1;
            } else {
                i_2 = i_2 + 1;
            }
        }
        Object pairs = (java.util.Map<String,Object>[])new java.util.Map[]{};
        for (String t : order) {
            pairs = appendObj(pairs, new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("expr", (Object)(t)), java.util.Map.entry("count", (Object)(((int)(freq).getOrDefault(t, 0)))))));
        }
        pairs = sortPairs(((java.util.Map<String,Object>[])(pairs)));
        int idx = 0;
        while (idx < String.valueOf(pairs).length() && idx < 10) {
            Object p_1 = pairs[idx];
            System.out.println((String)(((Object)(((java.util.Map)p_1)).get("expr"))) + " " + _p(((Object)(((java.util.Map)p_1)).get("count"))));
            idx = idx + 1;
        }
    }
    public static void main(String[] args) {
        main();
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
