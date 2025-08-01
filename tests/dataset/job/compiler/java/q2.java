// Generated by Mochi compiler v0.10.25 on 2025-07-13T10:54:08Z
// q2.mochi
import java.util.*;

class IdCountryCode {
    int id;
    String country_code;
    IdCountryCode(int id, String country_code) {
        this.id = id;
        this.country_code = country_code;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdCountryCode other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.country_code, other.country_code);
    }
    @Override public int hashCode() {
        return Objects.hash(id, country_code);
    }
    int size() { return 2; }
}
class IdKeyword {
    int id;
    String keyword;
    IdKeyword(int id, String keyword) {
        this.id = id;
        this.keyword = keyword;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdKeyword other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.keyword, other.keyword);
    }
    @Override public int hashCode() {
        return Objects.hash(id, keyword);
    }
    int size() { return 2; }
}
class MovieIdCompanyId {
    int movie_id;
    int company_id;
    MovieIdCompanyId(int movie_id, int company_id) {
        this.movie_id = movie_id;
        this.company_id = company_id;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof MovieIdCompanyId other)) return false;
        return Objects.equals(this.movie_id, other.movie_id) && Objects.equals(this.company_id, other.company_id);
    }
    @Override public int hashCode() {
        return Objects.hash(movie_id, company_id);
    }
    int size() { return 2; }
}
class MovieIdKeywordId {
    int movie_id;
    int keyword_id;
    MovieIdKeywordId(int movie_id, int keyword_id) {
        this.movie_id = movie_id;
        this.keyword_id = keyword_id;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof MovieIdKeywordId other)) return false;
        return Objects.equals(this.movie_id, other.movie_id) && Objects.equals(this.keyword_id, other.keyword_id);
    }
    @Override public int hashCode() {
        return Objects.hash(movie_id, keyword_id);
    }
    int size() { return 2; }
}
class IdTitle {
    int id;
    String title;
    IdTitle(int id, String title) {
        this.id = id;
        this.title = title;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IdTitle other)) return false;
        return Objects.equals(this.id, other.id) && Objects.equals(this.title, other.title);
    }
    @Override public int hashCode() {
        return Objects.hash(id, title);
    }
    int size() { return 2; }
}
public class Q2 {
    static Map<String,Object> asMap(Object o) {
        if (o instanceof Map<?,?> mm) {
            LinkedHashMap<String,Object> m = new LinkedHashMap<>();
            for (Map.Entry<?,?> e : mm.entrySet()) m.put(String.valueOf(e.getKey()), e.getValue());
            return m;
        }
        LinkedHashMap<String,Object> m = new LinkedHashMap<>();
        for (var f : o.getClass().getDeclaredFields()) { try { f.setAccessible(true); m.put(f.getName(), f.get(o)); } catch (Exception e) { throw new RuntimeException(e); } }
        return m;
    }
    static void saveJsonl(List<?> list) {
        for (Object obj : list) {
            Map<String,Object> m = asMap(obj);
            List<String> parts = new ArrayList<>();
            for (Map.Entry<?,?> e : m.entrySet()) { parts.add("\"" + e.getKey() + "\":" + e.getValue()); }
            System.out.println("{" + String.join(",", parts) + "}");
        }
    }
    static String toJson(Object o) {
        if (o instanceof Map<?,?> m) {
            StringJoiner j = new StringJoiner(",", "{", "}");
            for (Map.Entry<?,?> e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + toJson(e.getValue()));
            return j.toString();
        } else if (o instanceof Collection<?> c) {
            StringJoiner j = new StringJoiner(",", "[", "]");
            for (var x : c) j.add(toJson(x));
            return j.toString();
        } else if (o instanceof String s) {
            return "\"" + s + "\"";
        } else if (o instanceof Number || o instanceof Boolean || o instanceof Character) {
            return String.valueOf(o);
        } else {
            Map<String,Object> m = asMap(o);
            StringJoiner j = new StringJoiner(",", "{", "}");
            for (Map.Entry<String,Object> e : m.entrySet()) j.add("\"" + e.getKey() + "\":" + toJson(e.getValue()));
            return j.toString();
        }
    }
    static void json(Object o) { System.out.println(toJson(o)); }
    public static void main(String[] args) {
    List<IdCountryCode> company_name = new ArrayList<>(Arrays.asList(new IdCountryCode(1, "[de]"), new IdCountryCode(2, "[us]")));
    List<IdKeyword> keyword = new ArrayList<>(Arrays.asList(new IdKeyword(1, "character-name-in-title"), new IdKeyword(2, "other")));
    List<MovieIdCompanyId> movie_companies = new ArrayList<>(Arrays.asList(new MovieIdCompanyId(100, 1), new MovieIdCompanyId(200, 2)));
    List<MovieIdKeywordId> movie_keyword = new ArrayList<>(Arrays.asList(new MovieIdKeywordId(100, 1), new MovieIdKeywordId(200, 2)));
    List<IdTitle> title = new ArrayList<>(Arrays.asList(new IdTitle(100, "Der Film"), new IdTitle(200, "Other Movie")));
    List<String> titles = (new java.util.function.Supplier<List<String>>(){public List<String> get(){
    List<String> res0 = new ArrayList<>();
    for (var cn : company_name) {
        for (var mc : movie_companies) {
            if (!(mc.company_id == cn.id)) continue;
            for (var t : title) {
                if (!(mc.movie_id == t.id)) continue;
                for (var mk : movie_keyword) {
                    if (!(mk.movie_id == t.id)) continue;
                    for (var k : keyword) {
                        if (!(mk.keyword_id == k.id)) continue;
                        if (!(Objects.equals(cn.country_code, "[de]") && Objects.equals(k.keyword, "character-name-in-title") && mc.movie_id == mk.movie_id)) continue;
                        res0.add(t.title);
                    }
                }
            }
        }
    }
    return res0;
}}).get();
    int result = titles.stream().mapToInt(n -> ((Number)n).intValue()).min().orElse(Integer.MAX_VALUE);
    json(result);
    if (!(Objects.equals(result, "Der Film"))) throw new AssertionError("expect failed");
    }
}
