public class Main {

    static String[] split(String s, String sep) {
        String[] out = new String[]{};
        String cur = "";
        int i = 0;
        while (i < _runeLen(s)) {
            if (i + _runeLen(sep) <= _runeLen(s) && (_substr(s, i, i + _runeLen(sep)).equals(sep))) {
                out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                cur = "";
                i = i + _runeLen(sep);
            } else {
                cur = cur + _substr(s, i, i + 1);
                i = i + 1;
            }
        }
        out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        return out;
    }

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

    static String trimLeftSpaces(String s) {
        int i = 0;
        while (i < _runeLen(s) && (s.substring(i, i + 1).equals(" "))) {
            i = i + 1;
        }
        return s.substring(i, _runeLen(s));
    }

    static java.util.Map<String,Object>[] makeIndent(String outline, int tab) {
        String[] lines = outline.split("\n");
        java.util.Map<String,Object>[] nodes = (java.util.Map<String,Object>[])new java.util.Map[]{};
        for (String line : lines) {
            String line2 = String.valueOf(trimLeftSpaces(line));
            int level = (_runeLen(line) - _runeLen(line2)) / tab;
            nodes = appendObj(nodes, new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("level", level), java.util.Map.entry("name", line2))));
        }
        return nodes;
    }

    static void toNest(java.util.Map<String,Object>[] nodes, int start, int level, java.util.Map<String,Object> n) {
        if (level == 0) {
n.put("name", (Object)(((Object)(nodes[0]).get("name"))));
        }
        int i = start + 1;
        while (i < nodes.length) {
            java.util.Map<String,Object> node = nodes[i];
            int lev = (int)(((int)(node).getOrDefault("level", 0)));
            if (lev == level + 1) {
                java.util.Map child = new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("name", ((Object)(node).get("name"))), java.util.Map.entry("children", new Object[]{})));
                toNest(nodes, i, level + 1, child);
                Object[] cs = (Object[])(((Object[])(n).get("children")));
                cs = java.util.stream.Stream.concat(java.util.Arrays.stream(cs), java.util.stream.Stream.of(child)).toArray(java.util.Map[]::new);
n.put("children", cs);
            } else             if (lev <= level) {
                return;
            }
            i = i + 1;
        }
    }

    static int countLeaves(java.util.Map<String,Object> n) {
        Object[] kids = (Object[])(((Object[])(n).get("children")));
        if (kids.length == 0) {
            return 1;
        }
        int total = 0;
        for (Object k : kids) {
            total = total + countLeaves(((java.util.Map<String,Object>)(k)));
        }
        return total;
    }

    static java.util.Map<String,Object>[][] nodesByDepth(java.util.Map<String,Object> root, int depth) {
        java.util.Map<String,Object>[][] levels = new java.util.Map[][]{};
        java.util.Map<String,Object>[] current = (java.util.Map<String,Object>[])new java.util.Map[]{root};
        int d = 0;
        while (d < depth) {
            levels = appendObj(levels, current);
            java.util.Map<String,Object>[] next = (java.util.Map<String,Object>[])new java.util.Map[]{};
            for (java.util.Map<String,Object> n : current) {
                Object[] kids = (Object[])(((Object[])(n).get("children")));
                for (Object k : kids) {
                    next = appendObj(next, ((java.util.Map<String,Object>)(k)));
                }
            }
            current = next;
            d = d + 1;
        }
        return levels;
    }

    static String toMarkup(java.util.Map<String,Object> n, String[] cols, int depth) {
        String[] lines = new String[]{};
        lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of("{| class=\"wikitable\" style=\"text-align: center;\"")).toArray(String[]::new);
        String l1 = "|-";
        lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(l1)).toArray(String[]::new);
        int span = countLeaves(n);
        lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of("| style=\"background: " + cols[0] + " \" colSpan=" + String.valueOf(span) + " | " + String.valueOf((((String)(n).get("name")))))).toArray(String[]::new);
        lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(l1)).toArray(String[]::new);
        java.util.Map<String,Object>[][] lvls = nodesByDepth(n, depth);
        int lvl = 1;
        while (lvl < depth) {
            java.util.Map<String,Object>[] nodes = lvls[lvl];
            if (nodes.length == 0) {
                lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of("|  |")).toArray(String[]::new);
            } else {
                int idx = 0;
                while (idx < nodes.length) {
                    java.util.Map<String,Object> node = nodes[idx];
                    span = countLeaves(node);
                    int col = lvl;
                    if (lvl == 1) {
                        col = idx + 1;
                    }
                    if (col >= cols.length) {
                        col = cols.length - 1;
                    }
                    String cell = "| style=\"background: " + cols[col] + " \" colspan=" + String.valueOf(span) + " | " + String.valueOf((((String)(node).get("name"))));
                    lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(cell)).toArray(String[]::new);
                    idx = idx + 1;
                }
            }
            if (lvl < depth - 1) {
                lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of(l1)).toArray(String[]::new);
            }
            lvl = lvl + 1;
        }
        lines = java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of("|}")).toArray(String[]::new);
        return join(lines, "\n");
    }

    static void main() {
        String outline = "Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n" + "        measuring the indent of each line,\n" + "        translating the indentation to a nested structure,\n" + "        and padding the tree to even depth.\n" + "    count the leaves descending from each node,\n" + "        defining the width of a leaf as 1,\n" + "        and the width of a parent node as a sum.\n" + "            (The sum of the widths of its children)\n" + "    and write out a table with 'colspan' values\n" + "        either as a wiki table,\n" + "        or as HTML.";
        String yellow = "#ffffe6;";
        String orange = "#ffebd2;";
        String green = "#f0fff0;";
        String blue = "#e6ffff;";
        String pink = "#ffeeff;";
        String[] cols = new String[]{yellow, orange, green, blue, pink};
        java.util.Map<String,Object>[] nodes = makeIndent(outline, 4);
        java.util.Map n = new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("name", ""), java.util.Map.entry("children", new Object[]{})));
        toNest(nodes, 0, 0, n);
        System.out.println(toMarkup(n, cols, 4));
        System.out.println("\n");
        String outline2 = "Display an outline as a nested table.\n" + "    Parse the outline to a tree,\n" + "        measuring the indent of each line,\n" + "        translating the indentation to a nested structure,\n" + "        and padding the tree to even depth.\n" + "    count the leaves descending from each node,\n" + "        defining the width of a leaf as 1,\n" + "        and the width of a parent node as a sum.\n" + "            (The sum of the widths of its children)\n" + "            Propagating the sums upward as necessary.\n" + "    and write out a table with 'colspan' values\n" + "        either as a wiki table,\n" + "        or as HTML.\n" + "    Optionally add color to the nodes.";
        String[] cols2 = new String[]{blue, yellow, orange, green, pink};
        java.util.Map<String,Object>[] nodes2 = makeIndent(outline2, 4);
        java.util.Map n2 = new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("name", ""), java.util.Map.entry("children", new Object[]{})));
        toNest(nodes2, 0, 0, n2);
        System.out.println(toMarkup(n2, cols2, 4));
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
}
