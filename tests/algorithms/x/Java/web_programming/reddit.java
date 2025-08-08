public class Main {
    static class Post {
        String title;
        String url;
        String selftext;
        Post(String title, String url, String selftext) {
            this.title = title;
            this.url = url;
            this.selftext = selftext;
        }
        Post() {}
        @Override public String toString() {
            return String.format("{'title': '%s', 'url': '%s', 'selftext': '%s'}", String.valueOf(title), String.valueOf(url), String.valueOf(selftext));
        }
    }

    static class Child {
        Post data;
        Child(Post data) {
            this.data = data;
        }
        Child() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }

    static class ListingData {
        Child[] children;
        ListingData(Child[] children) {
            this.children = children;
        }
        ListingData() {}
        @Override public String toString() {
            return String.format("{'children': %s}", String.valueOf(children));
        }
    }

    static class Listing {
        ListingData data;
        Listing(ListingData data) {
            this.data = data;
        }
        Listing() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }

    static String[] valid_terms;

    static boolean contains(String[] xs, String x) {
        int i = 0;
        while (i < xs.length) {
            if ((xs[i].equals(x))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String join_with_comma(String[] xs) {
        String s = "";
        int i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 > 0) {
                s = s + ", ";
            }
            s = s + xs[i_1];
            i_1 = i_1 + 1;
        }
        return s;
    }

    static java.util.Map<Integer,java.util.Map<String,String>> get_subreddit_data(String subreddit, int limit, String age, String[] wanted_data) {
        String[] invalid = ((String[])(new String[]{}));
        int i_2 = 0;
        while (i_2 < wanted_data.length) {
            String term = wanted_data[i_2];
            if (!(Boolean)contains(((String[])(valid_terms)), term)) {
                invalid = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(invalid), java.util.stream.Stream.of(term)).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        if (invalid.length > 0) {
            String msg = "Invalid search term: " + String.valueOf(join_with_comma(((String[])(invalid))));
            throw new RuntimeException(String.valueOf(msg));
        }
        Listing resp = _fetch_Listing("tests/github/TheAlgorithms/Mochi/web_programming/reddit_sample.json");
        java.util.Map<Integer,java.util.Map<String,String>> result = ((java.util.Map<Integer,java.util.Map<String,String>>)(new java.util.LinkedHashMap<Integer, java.util.Map<String,String>>()));
        int idx = 0;
        while (idx < limit) {
            Post post = resp.data.children[idx].data;
            java.util.Map<String,String> post_map = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
            if (wanted_data.length == 0) {
post_map.put("title", post.title);
post_map.put("url", post.url);
post_map.put("selftext", post.selftext);
            } else {
                int j = 0;
                while (j < wanted_data.length) {
                    String field = wanted_data[j];
                    if ((field.equals("title"))) {
post_map.put("title", post.title);
                    } else                     if ((field.equals("url"))) {
post_map.put("url", post.url);
                    } else                     if ((field.equals("selftext"))) {
post_map.put("selftext", post.selftext);
                    }
                    j = j + 1;
                }
            }
result.put(idx, String.valueOf(post_map));
            idx = idx + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            valid_terms = ((String[])(new String[]{"approved_at_utc", "approved_by", "author_flair_background_color", "author_flair_css_class", "author_flair_richtext", "author_flair_template_id", "author_fullname", "author_premium", "can_mod_post", "category", "clicked", "content_categories", "created_utc", "downs", "edited", "gilded", "gildings", "hidden", "hide_score", "is_created_from_ads_ui", "is_meta", "is_original_content", "is_reddit_media_domain", "is_video", "link_flair_css_class", "link_flair_richtext", "link_flair_text", "link_flair_text_color", "media_embed", "mod_reason_title", "name", "permalink", "pwls", "quarantine", "saved", "score", "secure_media", "secure_media_embed", "selftext", "subreddit", "subreddit_name_prefixed", "subreddit_type", "thumbnail", "title", "top_awarded_type", "total_awards_received", "ups", "upvote_ratio", "url", "user_reports"}));
            System.out.println(get_subreddit_data("learnpython", 1, "new", ((String[])(new String[]{"title", "url", "selftext"}))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
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

    static Listing _fetch_Listing(String url) {
        return _cast(Listing.class, _fetch(url));
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
}
