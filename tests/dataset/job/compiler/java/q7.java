public class Main {
    static void test_Q7_finds_movie_features_biography_for_person() {
        expect((result == new Object[]{new java.util.HashMap<>(java.util.Map.of("of_person", "Alan Brown", "biography_movie", "Feature Film"))}));
    }
    
    static Object[] aka_name = new Object[]{new java.util.HashMap<>(java.util.Map.of("person_id", 1, "name", "Anna Mae")), new java.util.HashMap<>(java.util.Map.of("person_id", 2, "name", "Chris"))};
    
    static Object[] cast_info = new Object[]{new java.util.HashMap<>(java.util.Map.of("person_id", 1, "movie_id", 10)), new java.util.HashMap<>(java.util.Map.of("person_id", 2, "movie_id", 20))};
    
    static Object[] info_type = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1, "info", "mini biography")), new java.util.HashMap<>(java.util.Map.of("id", 2, "info", "trivia"))};
    
    static Object[] link_type = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1, "link", "features")), new java.util.HashMap<>(java.util.Map.of("id", 2, "link", "references"))};
    
    static Object[] movie_link = new Object[]{new java.util.HashMap<>(java.util.Map.of("linked_movie_id", 10, "link_type_id", 1)), new java.util.HashMap<>(java.util.Map.of("linked_movie_id", 20, "link_type_id", 2))};
    
    static Object[] name = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1, "name", "Alan Brown", "name_pcode_cf", "B", "gender", "m")), new java.util.HashMap<>(java.util.Map.of("id", 2, "name", "Zoe", "name_pcode_cf", "Z", "gender", "f"))};
    
    static Object[] person_info = new Object[]{new java.util.HashMap<>(java.util.Map.of("person_id", 1, "info_type_id", 1, "note", "Volker Boehm")), new java.util.HashMap<>(java.util.Map.of("person_id", 2, "info_type_id", 1, "note", "Other"))};
    
    static Object[] title = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 10, "title", "Feature Film", "production_year", 1990)), new java.util.HashMap<>(java.util.Map.of("id", 20, "title", "Late Film", "production_year", 2000))};
    
    static Object[] rows = (new java.util.function.Supplier<java.util.List<Object>>() {
    public java.util.List<Object> get() {
        java.util.List<Object> _src = _toList(aka_name);
        java.util.List<_JoinSpec> _joins = java.util.List.of(
            new _JoinSpec(_toList(name), (Object[] a) -> { Object an = a[0]; Object n = a[1]; return (n.get("id") == an.get("person_id")); }, false, false),
            new _JoinSpec(_toList(person_info), (Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; return (pi.get("person_id") == an.get("person_id")); }, false, false),
            new _JoinSpec(_toList(info_type), (Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; Object it = a[3]; return (it.get("id") == pi.get("info_type_id")); }, false, false),
            new _JoinSpec(_toList(cast_info), (Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; Object it = a[3]; Object ci = a[4]; return (ci.get("person_id") == n.get("id")); }, false, false),
            new _JoinSpec(_toList(title), (Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; Object it = a[3]; Object ci = a[4]; Object t = a[5]; return (t.get("id") == ci.get("movie_id")); }, false, false),
            new _JoinSpec(_toList(movie_link), (Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; Object it = a[3]; Object ci = a[4]; Object t = a[5]; Object ml = a[6]; return (ml.get("linked_movie_id") == t.get("id")); }, false, false),
            new _JoinSpec(_toList(link_type), (Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; Object it = a[3]; Object ci = a[4]; Object t = a[5]; Object ml = a[6]; Object lt = a[7]; return (lt.get("id") == ml.get("link_type_id")); }, false, false)
        );
        return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; Object it = a[3]; Object ci = a[4]; Object t = a[5]; Object ml = a[6]; Object lt = a[7]; return new java.util.HashMap<>(java.util.Map.of("person_name", n.get("name"), "movie_title", t.get("title"))); }, (Object[] a) -> { Object an = a[0]; Object n = a[1]; Object pi = a[2]; Object it = a[3]; Object ci = a[4]; Object t = a[5]; Object ml = a[6]; Object lt = a[7]; return ((((((((((((an.get("name").contains("a") && (it.get("info") == "mini biography")) && (lt.get("link") == "features")) && (n.get("name_pcode_cf") >= "A")) && (n.get("name_pcode_cf") <= "F")) && ((n.get("gender") == "m") || ((n.get("gender") == "f") && n.get("name").starts_with("B")))) && (pi.get("note") == "Volker Boehm")) && (t.get("production_year") >= 1980)) && (t.get("production_year") <= 1995)) && (pi.get("person_id") == an.get("person_id"))) && (pi.get("person_id") == ci.get("person_id"))) && (an.get("person_id") == ci.get("person_id"))) && (ci.get("movie_id") == ml.get("linked_movie_id"))); }, null, -1, -1));
    }
}).get();
    
    static Object[] result = new Object[]{new java.util.HashMap<>(java.util.Map.of("of_person", min.apply((new java.util.function.Supplier<java.util.List<Object>>() {
    public java.util.List<Object> get() {
        java.util.List<Object> _src = _toList(rows);
        java.util.List<_JoinSpec> _joins = java.util.List.of(
        );
        return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object r = a[0]; return r.get("person_name"); }, null, null, -1, -1));
    }
}).get()), "biography_movie", min.apply((new java.util.function.Supplier<java.util.List<Object>>() {
    public java.util.List<Object> get() {
        java.util.List<Object> _src = _toList(rows);
        java.util.List<_JoinSpec> _joins = java.util.List.of(
        );
        return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object r = a[0]; return r.get("movie_title"); }, null, null, -1, -1));
    }
}).get())))};
    
    public static void main(String[] args) {
        test_Q7_finds_movie_features_biography_for_person();
        _json(result);
    }
    
    static void expect(boolean cond) {
        if (!cond) throw new RuntimeException("expect failed");
    }
    
    static void _json(Object v) {
        System.out.println(_toJson(v));
    }
    
    static java.util.List<Object> _toList(Object v) {
        if (v instanceof java.util.List<?>) return new java.util.ArrayList<>((java.util.List<?>)v);
        int n = java.lang.reflect.Array.getLength(v);
        java.util.List<Object> out = new java.util.ArrayList<>(n);
        for (int i=0;i<n;i++) out.add(java.lang.reflect.Array.get(v,i));
        return out;
    }
    
    static class _JoinSpec {
        java.util.List<Object> items;
        java.util.function.Function<Object[],Boolean> on;
        boolean left;
        boolean right;
        _JoinSpec(java.util.List<Object> items, java.util.function.Function<Object[],Boolean> on, boolean left, boolean right) {
            this.items=items; this.on=on; this.left=left; this.right=right;
        }
    }
    
    static class _QueryOpts {
        java.util.function.Function<Object[],Object> selectFn;
        java.util.function.Function<Object[],Boolean> where;
        java.util.function.Function<Object[],Object> sortKey;
        int skip; int take;
        _QueryOpts(java.util.function.Function<Object[],Object> s, java.util.function.Function<Object[],Boolean> w, java.util.function.Function<Object[],Object> k, int skip, int take) {
            this.selectFn=s; this.where=w; this.sortKey=k; this.skip=skip; this.take=take;
        }
    }
    static java.util.List<Object> _query(java.util.List<Object> src, java.util.List<_JoinSpec> joins, _QueryOpts opts) {
        java.util.List<java.util.List<Object>> items = new java.util.ArrayList<>();
        for (Object v : src) { java.util.List<Object> r = new java.util.ArrayList<>(); r.add(v); items.add(r); }
        for (_JoinSpec j : joins) {
            java.util.List<java.util.List<Object>> joined = new java.util.ArrayList<>();
            java.util.List<Object> jitems = j.items;
            if (j.right && j.left) {
                boolean[] matched = new boolean[jitems.size()];
                for (java.util.List<Object> left : items) {
                    boolean m = false;
                    for (int ri=0; ri<jitems.size(); ri++) {
                        Object right = jitems.get(ri);
                        boolean keep = true;
                        if (j.on != null) {
                            Object[] args = new Object[left.size()+1];
                            for (int i=0;i<left.size();i++) args[i]=left.get(i);
                            args[left.size()] = right;
                            keep = j.on.apply(args);
                        }
                        if (!keep) continue;
                        m = true; matched[ri] = true;
                        java.util.List<Object> row = new java.util.ArrayList<>(left);
                        row.add(right); joined.add(row);
                    }
                    if (!m) { java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(null); joined.add(row); }
                }
                for (int ri=0; ri<jitems.size(); ri++) {
                    if (!matched[ri]) { java.util.List<Object> undef = new java.util.ArrayList<>(items.isEmpty()?0:items.get(0).size()); for(int k=0;k<undef.size();k++) undef.set(k,null); undef.add(jitems.get(ri)); joined.add(undef); }
                }
            } else if (j.right) {
                for (Object right : jitems) {
                    boolean m = false;
                    for (java.util.List<Object> left : items) {
                        boolean keep = true;
                        if (j.on != null) {
                            Object[] args = new Object[left.size()+1];
                            for (int i=0;i<left.size();i++) args[i]=left.get(i);
                            args[left.size()] = right;
                            keep = j.on.apply(args);
                        }
                        if (!keep) continue;
                        m = true; java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(right); joined.add(row);
                    }
                    if (!m) { java.util.List<Object> undef = new java.util.ArrayList<>(items.isEmpty()?0:items.get(0).size()); for(int k=0;k<undef.size();k++) undef.set(k,null); undef.add(right); joined.add(undef); }
                }
            } else {
                for (java.util.List<Object> left : items) {
                    boolean m = false;
                    for (Object right : jitems) {
                        boolean keep = true;
                        if (j.on != null) {
                            Object[] args = new Object[left.size()+1];
                            for (int i=0;i<left.size();i++) args[i]=left.get(i);
                            args[left.size()] = right;
                            keep = j.on.apply(args);
                        }
                        if (!keep) continue;
                        m = true; java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(right); joined.add(row);
                    }
                    if (j.left && !m) { java.util.List<Object> row = new java.util.ArrayList<>(left); row.add(null); joined.add(row); }
                }
            items = joined;
        }
        if (opts.where != null) {
            java.util.List<java.util.List<Object>> filtered = new java.util.ArrayList<>();
            for (java.util.List<Object> r : items) if (opts.where.apply(r.toArray(new Object[0]))) filtered.add(r);
            items = filtered;
        }
        if (opts.sortKey != null) {
            class Pair { java.util.List<Object> item; Object key; Pair(java.util.List<Object> i,Object k){item=i;key=k;} }
            java.util.List<Pair> pairs = new java.util.ArrayList<>();
            for (java.util.List<Object> it : items) pairs.add(new Pair(it, opts.sortKey.apply(it.toArray(new Object[0]))));
            pairs.sort((a,b) -> {
                Object ak=a.key, bk=b.key;
                if (ak instanceof Number && bk instanceof Number) return Double.compare(((Number)ak).doubleValue(), ((Number)bk).doubleValue());
                if (ak instanceof String && bk instanceof String) return ((String)ak).compareTo((String)bk);
                return ak.toString().compareTo(bk.toString());
            });
            for (int i=0;i<pairs.size();i++) items.set(i, pairs.get(i).item);
        }
        if (opts.skip >= 0) { if (opts.skip < items.size()) items = new java.util.ArrayList<>(items.subList(opts.skip, items.size())); else items = new java.util.ArrayList<>(); }
        if (opts.take >= 0) { if (opts.take < items.size()) items = new java.util.ArrayList<>(items.subList(0, opts.take)); }
        java.util.List<Object> res = new java.util.ArrayList<>();
        for (java.util.List<Object> r : items) res.add(opts.selectFn.apply(r.toArray(new Object[0])));
        return res;
    }
}
