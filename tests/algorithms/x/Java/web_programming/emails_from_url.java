public class Main {
    static class Page {
        String url;
        String html;
        Page(String url, String html) {
            this.url = url;
            this.html = html;
        }
        Page() {}
        @Override public String toString() {
            return String.format("{'url': '%s', 'html': '%s'}", String.valueOf(url), String.valueOf(html));
        }
    }

    static Page[] pages;
    static String[] emails_2;
    static int k = 0;

    static int index_of(String s, String ch) {
        int i = 0;
        while (i < _runeLen(s)) {
            if ((s.substring(i, i+1).equals(ch))) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int index_of_substring(String s, String sub) {
        int n = _runeLen(s);
        int m = _runeLen(sub);
        if (m == 0) {
            return 0;
        }
        int i_1 = 0;
        while (i_1 <= n - m) {
            int j = 0;
            boolean is_match = true;
            while (j < m) {
                if (!(s.substring(i_1 + j, i_1 + j+1).equals(sub.substring(j, j+1)))) {
                    is_match = false;
                    break;
                }
                j = j + 1;
            }
            if (is_match) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return -1;
    }

    static String[] split(String s, String sep) {
        String[] parts = ((String[])(new String[]{}));
        int last = 0;
        int i_2 = 0;
        while (i_2 < _runeLen(s)) {
            String ch = s.substring(i_2, i_2+1);
            if ((ch.equals(sep))) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(_substr(s, last, i_2))).toArray(String[]::new)));
                last = i_2 + 1;
            }
            if (i_2 + 1 == _runeLen(s)) {
                parts = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parts), java.util.stream.Stream.of(_substr(s, last, i_2 + 1))).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        return parts;
    }

    static String get_sub_domain_name(String url) {
        int proto_pos = index_of_substring(url, "://");
        int start = 0;
        if (proto_pos >= 0) {
            start = proto_pos + 3;
        }
        int i_3 = start;
        while (i_3 < _runeLen(url)) {
            if ((url.substring(i_3, i_3+1).equals("/"))) {
                break;
            }
            i_3 = i_3 + 1;
        }
        return _substr(url, start, i_3);
    }

    static String get_domain_name(String url) {
        String sub = String.valueOf(get_sub_domain_name(url));
        String[] parts_1 = ((String[])(sub.split(java.util.regex.Pattern.quote("."))));
        if (parts_1.length >= 2) {
            return parts_1[parts_1.length - 2] + "." + parts_1[parts_1.length - 1];
        }
        return sub;
    }

    static boolean is_alnum(String ch) {
        String chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
        return index_of(chars, ch) >= 0;
    }

    static boolean contains(String[] xs, String x) {
        int i_4 = 0;
        while (i_4 < xs.length) {
            if ((xs[i_4].equals(x))) {
                return true;
            }
            i_4 = i_4 + 1;
        }
        return false;
    }

    static String[] bubble_sort(String[] xs) {
        String[] arr = ((String[])(xs));
        int n_1 = arr.length;
        int i_5 = 0;
        while (i_5 < n_1) {
            int j_1 = 0;
            while (j_1 + 1 < n_1 - i_5) {
                if ((arr[j_1].compareTo(arr[j_1 + 1]) > 0)) {
                    String tmp = arr[j_1];
arr[j_1] = arr[j_1 + 1];
arr[j_1 + 1] = tmp;
                }
                j_1 = j_1 + 1;
            }
            i_5 = i_5 + 1;
        }
        return arr;
    }

    static String[] extract_links(String domain, String html) {
        String[] links = ((String[])(new String[]{}));
        int pos = index_of_substring(html, "href=");
        while (pos >= 0) {
            int start_quote = index_of(_substr(html, pos + 5, _runeLen(html)), "\"");
            if (start_quote < 0) {
                break;
            }
            int rest = pos + 5 + start_quote + 1;
            int end_quote = index_of(_substr(html, rest, _runeLen(html)), "\"");
            if (end_quote < 0) {
                break;
            }
            String link = _substr(html, rest, rest + end_quote);
            if (!(Boolean)contains(((String[])(links)), link)) {
                String absolute = link;
                if (!(index_of_substring(link, "http://") == 0 || index_of_substring(link, "https://") == 0)) {
                    if (index_of_substring(link, "/") == 0) {
                        absolute = "https://" + domain + link;
                    } else {
                        absolute = "https://" + domain + "/" + link;
                    }
                }
                links = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(links), java.util.stream.Stream.of(absolute)).toArray(String[]::new)));
            }
            pos = index_of_substring(_substr(html, rest + end_quote, _runeLen(html)), "href=");
            if (pos >= 0) {
                pos = pos + rest + end_quote;
            }
        }
        return links;
    }

    static String[] extract_emails(String domain, String text) {
        String[] emails = ((String[])(new String[]{}));
        int i_6 = 0;
        while (i_6 < _runeLen(text)) {
            if ((text.substring(i_6, i_6+1).equals("@"))) {
                if ((_substr(text, i_6 + 1, i_6 + 1 + _runeLen(domain)).equals(domain))) {
                    int j_2 = i_6 - 1;
                    while (j_2 >= 0 && ((Boolean)(is_alnum(text.substring(j_2, j_2+1))))) {
                        j_2 = j_2 - 1;
                    }
                    String local = _substr(text, j_2 + 1, i_6);
                    if (_runeLen(local) > 0) {
                        String email = local + "@" + domain;
                        if (!(Boolean)contains(((String[])(emails)), email)) {
                            emails = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(emails), java.util.stream.Stream.of(email)).toArray(String[]::new)));
                        }
                    }
                }
            }
            i_6 = i_6 + 1;
        }
        return emails;
    }

    static String find_page(Page[] pages, String url) {
        int i_7 = 0;
        while (i_7 < pages.length) {
            Page p = pages[i_7];
            if ((p.url.equals(url))) {
                return p.html;
            }
            i_7 = i_7 + 1;
        }
        return "";
    }

    static String[] emails_from_url(String url, Page[] pages) {
        String domain = String.valueOf(get_domain_name(url));
        String base_html = String.valueOf(find_page(((Page[])(pages)), url));
        String[] links_1 = ((String[])(extract_links(domain, base_html)));
        String[] found = ((String[])(new String[]{}));
        int i_8 = 0;
        while (i_8 < links_1.length) {
            String html = String.valueOf(find_page(((Page[])(pages)), links_1[i_8]));
            String[] emails_1 = ((String[])(extract_emails(domain, html)));
            int j_3 = 0;
            while (j_3 < emails_1.length) {
                if (!(Boolean)contains(((String[])(found)), emails_1[j_3])) {
                    found = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(found), java.util.stream.Stream.of(emails_1[j_3])).toArray(String[]::new)));
                }
                j_3 = j_3 + 1;
            }
            i_8 = i_8 + 1;
        }
        String[] sorted = ((String[])(bubble_sort(((String[])(found)))));
        return sorted;
    }
    public static void main(String[] args) {
        pages = ((Page[])(new Page[]{new Page("https://example.com", "<html><body><a href=\"/contact\">Contact</a></body></html>"), new Page("https://example.com/contact", "<html>Contact us at info@example.com or support@example.com</html>")}));
        emails_2 = ((String[])(emails_from_url("https://example.com", ((Page[])(pages)))));
        System.out.println(_p(emails_2.length) + " emails found:");
        k = 0;
        while (k < emails_2.length) {
            System.out.println(emails_2[k]);
            k = k + 1;
        }
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
