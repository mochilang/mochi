public class Main {

    static int index_of_from(String s, String sub, int start) {
        int i = start;
        int max = _runeLen(s) - _runeLen(sub);
        while (i <= max) {
            if ((s.substring(i, i + _runeLen(sub)).equals(sub))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static java.util.Map<String,String>[] extract_links(String html) {
        java.util.Map<String,String>[] res = ((java.util.Map<String,String>[])((java.util.Map<String,String>[])new java.util.Map[]{}));
        int i_1 = 0;
        while (true) {
            int tag_start = index_of_from(html, "<a class=\"eZt8xd\"", i_1);
            if (tag_start == (-1)) {
                break;
            }
            int href_start = index_of_from(html, "href=\"", tag_start);
            if (href_start == (-1)) {
                break;
            }
            href_start = href_start + _runeLen("href=\"");
            int href_end = index_of_from(html, "\"", href_start);
            if (href_end == (-1)) {
                break;
            }
            String href = html.substring(href_start, href_end);
            int text_start = index_of_from(html, ">", href_end) + 1;
            int text_end = index_of_from(html, "</a>", text_start);
            if (text_end == (-1)) {
                break;
            }
            String text = html.substring(text_start, text_end);
            java.util.Map<String,String> link = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>(java.util.Map.ofEntries(java.util.Map.entry("href", href), java.util.Map.entry("text", text)))));
            res = ((java.util.Map<String,String>[])(appendObj(res, link)));
            i_1 = text_end + _runeLen("</a>");
        }
        return res;
    }

    static void main() {
        String html = "<div><a class=\"eZt8xd\" href=\"/url?q=http://example1.com\">Example1</a>" + "<a class=\"eZt8xd\" href=\"/maps\">Maps</a>" + "<a class=\"eZt8xd\" href=\"/url?q=http://example2.com\">Example2</a></div>";
        java.util.Map<String,String>[] links = ((java.util.Map<String,String>[])(extract_links(html)));
        System.out.println(_p(links.length));
        int i_2 = 0;
        while (i_2 < links.length && i_2 < 5) {
            java.util.Map<String,String> link_1 = links[i_2];
            String href_1 = ((String)(link_1).get("href"));
            String text_1 = ((String)(link_1).get("text"));
            if ((text_1.equals("Maps"))) {
                System.out.println(href_1);
            } else {
                System.out.println("https://google.com" + href_1);
            }
            i_2 = i_2 + 1;
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
