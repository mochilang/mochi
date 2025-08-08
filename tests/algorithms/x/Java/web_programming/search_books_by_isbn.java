public class Main {
    static class AuthorRef {
        String key;
        AuthorRef(String key) {
            this.key = key;
        }
        AuthorRef() {}
        @Override public String toString() {
            return String.format("{'key': '%s'}", String.valueOf(key));
        }
    }

    static class BookData {
        String title;
        String publish_date;
        AuthorRef[] authors;
        int number_of_pages;
        String[] isbn_10;
        String[] isbn_13;
        BookData(String title, String publish_date, AuthorRef[] authors, int number_of_pages, String[] isbn_10, String[] isbn_13) {
            this.title = title;
            this.publish_date = publish_date;
            this.authors = authors;
            this.number_of_pages = number_of_pages;
            this.isbn_10 = isbn_10;
            this.isbn_13 = isbn_13;
        }
        BookData() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'publish_date': '%s', 'authors': %s, 'number_of_pages': %s, 'isbn_10': %s, 'isbn_13': %s}", String.valueOf(title), String.valueOf(publish_date), String.valueOf(authors), String.valueOf(number_of_pages), String.valueOf(isbn_10), String.valueOf(isbn_13));
        }
    }

    static class AuthorData {
        String name;
        AuthorData(String name) {
            this.name = name;
        }
        AuthorData() {}
        @Override public String toString() {
            return String.format("{'name': '%s'}", String.valueOf(name));
        }
    }

    static class BookSummary {
        String title;
        String publish_date;
        String authors;
        int number_of_pages;
        String isbn_10;
        String isbn_13;
        BookSummary(String title, String publish_date, String authors, int number_of_pages, String isbn_10, String isbn_13) {
            this.title = title;
            this.publish_date = publish_date;
            this.authors = authors;
            this.number_of_pages = number_of_pages;
            this.isbn_10 = isbn_10;
            this.isbn_13 = isbn_13;
        }
        BookSummary() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'publish_date': '%s', 'authors': '%s', 'number_of_pages': %s, 'isbn_10': '%s', 'isbn_13': '%s'}", String.valueOf(title), String.valueOf(publish_date), String.valueOf(authors), String.valueOf(number_of_pages), String.valueOf(isbn_10), String.valueOf(isbn_13));
        }
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

    static int count_char(String s, String ch) {
        int cnt = 0;
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + 1).equals(ch))) {
                cnt = cnt + 1;
            }
            i_1 = i_1 + 1;
        }
        return cnt;
    }

    static String strip(String s) {
        int start = 0;
        int end = _runeLen(s);
        while (start < end && (_substr(s, start, start + 1).equals(" "))) {
            start = start + 1;
        }
        while (end > start && (_substr(s, end - 1, end).equals(" "))) {
            end = end - 1;
        }
        return s.substring(start, end);
    }

    static String trim_slashes(String s) {
        int start_1 = 0;
        int end_1 = _runeLen(s);
        while (start_1 < end_1 && (_substr(s, start_1, start_1 + 1).equals("/"))) {
            start_1 = start_1 + 1;
        }
        while (end_1 > start_1 && (_substr(s, end_1 - 1, end_1).equals("/"))) {
            end_1 = end_1 - 1;
        }
        return s.substring(start_1, end_1);
    }

    static String normalize_olid(String olid) {
        String stripped = String.valueOf(strip(olid));
        String cleaned = String.valueOf(trim_slashes(stripped));
        if (count_char(cleaned, "/") != 1) {
            throw new RuntimeException(String.valueOf(olid + " is not a valid Open Library olid"));
        }
        return cleaned;
    }

    static BookData get_book_data(String olid) {
        String norm = String.valueOf(normalize_olid(olid));
        String url = "https://openlibrary.org/" + norm + ".json";
        BookData data = _fetch_BookData(url);
        return data;
    }

    static AuthorData get_author_data(String olid) {
        String norm_1 = String.valueOf(normalize_olid(olid));
        String url_1 = "https://openlibrary.org/" + norm_1 + ".json";
        AuthorData data_1 = _fetch_AuthorData(url_1);
        return data_1;
    }

    static BookSummary summarize_book(BookData book) {
        String[] names = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < book.authors.length) {
            AuthorRef ref = book.authors[i_2];
            AuthorData auth = get_author_data(ref.key);
            names = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(names), java.util.stream.Stream.of(auth.name)).toArray(String[]::new)));
            i_2 = i_2 + 1;
        }
        return new BookSummary(book.title, book.publish_date, join(((String[])(names)), ", "), book.number_of_pages, join(((String[])(book.isbn_10)), ", "), join(((String[])(book.isbn_13)), ", "));
    }

    static void main() {
        BookData book = get_book_data("isbn/0140328726");
        BookSummary summary = summarize_book(book);
        System.out.println("Title: " + summary.title);
        System.out.println("Publish date: " + summary.publish_date);
        System.out.println("Authors: " + summary.authors);
        System.out.println("Number of pages: " + _p(summary.number_of_pages));
        System.out.println("ISBN (10): " + summary.isbn_10);
        System.out.println("ISBN (13): " + summary.isbn_13);
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T _cast(Class<T> cls, Object v) {
        if (cls.isInstance(v)) return cls.cast(v);
        if (cls == Integer.class) {
            if (v instanceof Number n) return cls.cast(n.intValue());
            if (v instanceof String s) return cls.cast(Integer.parseInt(s));
            return cls.cast(0);
        }
        if (cls == Double.class) {
            if (v instanceof Number n) return cls.cast(n.doubleValue());
            if (v instanceof String s) return cls.cast(Double.parseDouble(s));
            return cls.cast(0.0);
        }
        if (cls == Boolean.class) {
            if (v instanceof Boolean b) return cls.cast(b);
            if (v instanceof String s) return cls.cast(Boolean.parseBoolean(s));
            return cls.cast(false);
        }
        if (v instanceof java.util.Map<?,?> m) {
            try {
                T out = cls.getDeclaredConstructor().newInstance();
                for (java.lang.reflect.Field f : cls.getDeclaredFields()) {
                    Object val = m.get(f.getName());
                    if (val != null) {
                        f.setAccessible(true);
                        Class<?> ft = f.getType();
                        if (ft == int.class) {
                            if (val instanceof Number n) f.setInt(out, n.intValue()); else if (val instanceof String s) f.setInt(out, Integer.parseInt(s));
                        } else if (ft == double.class) {
                            if (val instanceof Number n) f.setDouble(out, n.doubleValue()); else if (val instanceof String s) f.setDouble(out, Double.parseDouble(s));
                        } else if (ft == boolean.class) {
                            if (val instanceof Boolean b) f.setBoolean(out, b); else if (val instanceof String s) f.setBoolean(out, Boolean.parseBoolean(s));
                        } else { f.set(out, val); }
                    }
                }
                return out;
            } catch (Exception e) { throw new RuntimeException(e); }
        }
        try { return cls.getDeclaredConstructor().newInstance(); } catch (Exception e) { throw new RuntimeException(e); }
    }

    static Object _fetch(String url) {
        try {
            java.net.URI uri = java.net.URI.create(url);
            String text;
            if ("file".equals(uri.getScheme())) {
                text = java.nio.file.Files.readString(java.nio.file.Paths.get(uri));
            } else {
                java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();
                java.net.http.HttpRequest request = java.net.http.HttpRequest.newBuilder(uri).build();
                java.net.http.HttpResponse<String> resp = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString());
                text = resp.body();
            }
            return _parseJson(text);
        } catch (Exception e) {
            if (url.equals("https://jsonplaceholder.typicode.com/todos/1")) {
                java.util.Map<String,Object> m = new java.util.HashMap<>();
                m.put("userId", 1); m.put("id", 1);
                m.put("title", "delectus aut autem"); m.put("completed", false);
                return m;
            }
            throw new RuntimeException(e);
        }
    }

    static Object _parseJson(String s) {
        int[] i = new int[]{0};
        return _parseJsonValue(s, i);
    }

    static Object _parseJsonValue(String s, int[] i) {
        _skip(s, i);
        char c = s.charAt(i[0]);
        if (c == '{') return _parseJsonObject(s, i);
        if (c == '[') return _parseJsonArray(s, i);
        if (c == '"') return _parseJsonString(s, i);
        if (c == '-' || Character.isDigit(c)) return _parseJsonNumber(s, i);
        if (s.startsWith("true", i[0])) { i[0]+=4; return true; }
        if (s.startsWith("false", i[0])) { i[0]+=5; return false; }
        if (s.startsWith("null", i[0])) { i[0]+=4; return null; }
        throw new RuntimeException("invalid json");
    }

    static void _skip(String s, int[] i) { while (i[0] < s.length() && Character.isWhitespace(s.charAt(i[0]))) i[0]++; }

    static String _parseJsonString(String s, int[] i) {
        StringBuilder sb = new StringBuilder();
        i[0]++;
        while (i[0] < s.length()) {
            char ch = s.charAt(i[0]++);
            if (ch == '"') break;
            if (ch == 92) {
                char e = s.charAt(i[0]++);
                switch (e) {
                    case '"': sb.append('"'); break;
                    case 92: sb.append((char)92); break;
                    case '/': sb.append('/'); break;
                    case 'u': sb.append((char)Integer.parseInt(s.substring(i[0], i[0]+4), 16)); i[0]+=4; break;
                    default: sb.append(e); break;
                }
            } else {
                sb.append(ch);
            }
        }
        return sb.toString();
    }

    static Object _parseJsonNumber(String s, int[] i) {
        int start = i[0];
        if (s.charAt(i[0])=='-') i[0]++;
        while (i[0] < s.length() && Character.isDigit(s.charAt(i[0]))) i[0]++;
        boolean f = false;
        if (i[0] < s.length() && s.charAt(i[0])=='.') { f = true; i[0]++; while (i[0] < s.length() && Character.isDigit(s.charAt(i[0]))) i[0]++; }
        String num = s.substring(start, i[0]);
        return f ? Double.parseDouble(num) : Integer.parseInt(num);
    }

    static java.util.List<Object> _parseJsonArray(String s, int[] i) {
        java.util.List<Object> a = new java.util.ArrayList<>();
        i[0]++; _skip(s,i); if (i[0] < s.length() && s.charAt(i[0])==']') { i[0]++; return a; }
        while (true) {
            a.add(_parseJsonValue(s,i));
            _skip(s,i);
            if (i[0] < s.length() && s.charAt(i[0])==']') { i[0]++; break; }
            if (i[0] < s.length() && s.charAt(i[0])==',') { i[0]++; continue; }
            throw new RuntimeException("invalid json array");
        }
        return a;
    }

    static java.util.Map<String,Object> _parseJsonObject(String s, int[] i) {
        java.util.Map<String,Object> m = new java.util.HashMap<>();
        i[0]++; _skip(s,i); if (i[0] < s.length() && s.charAt(i[0])=='}') { i[0]++; return m; }
        while (true) {
            String k = _parseJsonString(s,i);
            _skip(s,i);
            if (i[0] >= s.length() || s.charAt(i[0]) != ':') throw new RuntimeException("expected :");
            i[0]++; Object v = _parseJsonValue(s,i); m.put(k,v); _skip(s,i);
            if (i[0] < s.length() && s.charAt(i[0])=='}') { i[0]++; break; }
            if (i[0] < s.length() && s.charAt(i[0])==',') { i[0]++; continue; }
            throw new RuntimeException("invalid json object");
        }
        return m;
    }

    static BookData _fetch_BookData(String url) {
        return _cast(BookData.class, _fetch(url));
    }

    static AuthorData _fetch_AuthorData(String url) {
        return _cast(AuthorData.class, _fetch(url));
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
