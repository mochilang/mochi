public class Main {
    static String[] query_words;
    static String query = null;
    static int i_1 = 0;
    static String url;
    static String sample_html;
    static String link;

    static int index_of_substr(String s, String pat) {
        if (_runeLen(pat) == 0) {
            return 0;
        }
        int i = 0;
        while (i <= _runeLen(s) - _runeLen(pat)) {
            int j = 0;
            while (j < _runeLen(pat)) {
                if (!(s.substring(i + j, i + j+1).equals(pat.substring(j, j+1)))) {
                    break;
                }
                j = j + 1;
            }
            if (j == _runeLen(pat)) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static String first_link(String html) {
        int a_idx = index_of_substr(html, "<a");
        if (a_idx < 0) {
            return "";
        }
        String href_pat = "href=\"";
        int href_idx_rel = index_of_substr(html.substring(a_idx, _runeLen(html)), href_pat);
        if (href_idx_rel < 0) {
            return "";
        }
        int start = a_idx + href_idx_rel + _runeLen(href_pat);
        int end = start;
        while (end < _runeLen(html)) {
            if ((html.substring(end, end+1).equals("\""))) {
                break;
            }
            end = end + 1;
        }
        return html.substring(start, end);
    }
    public static void main(String[] args) {
        query_words = ((String[])(new String[]{"mochi", "language"}));
        query = "";
        i_1 = 0;
        while (i_1 < query_words.length) {
            if (i_1 > 0) {
                query = query + "%20";
            }
            query = query + query_words[i_1];
            i_1 = i_1 + 1;
        }
        url = "https://www.google.com/search?q=" + query + "&num=100";
        System.out.println("Googling.....");
        sample_html = "<div><a href=\"https://example.com\">Example</a></div>" + "<div><a href=\"https://another.com\">Another</a></div>";
        link = String.valueOf(first_link(sample_html));
        if (_runeLen(link) > 0) {
            System.out.println(link);
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
