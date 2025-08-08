public class Main {
    static class User {
        String login;
        int id;
        User(String login, int id) {
            this.login = login;
            this.id = id;
        }
        User() {}
        @Override public String toString() {
            return String.format("{'login': '%s', 'id': %s}", String.valueOf(login), String.valueOf(id));
        }
    }


    static User fetch_github_info(String username) {
        String url = "https://api.github.com/users/" + username;
        User res = _fetch_User(url);
        return res;
    }

    static void main() {
        User user = fetch_github_info("octocat");
        System.out.println(user.login + " " + _p(user.id));
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

    static User _fetch_User(String url) {
        return _cast(User.class, _fetch(url));
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
