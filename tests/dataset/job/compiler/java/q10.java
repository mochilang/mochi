public class Main {
    static void test_Q10_finds_uncredited_voice_actor_in_Russian_movie() {
        expect((result == new Object[]{new java.util.HashMap<>(java.util.Map.of("uncredited_voiced_character", "Ivan", "russian_movie", "Vodka Dreams"))}));
    }
    
    static Object[] char_name = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1, "name", "Ivan")), new java.util.HashMap<>(java.util.Map.of("id", 2, "name", "Alex"))};
    
    static Object[] cast_info = new Object[]{new java.util.HashMap<>(java.util.Map.of("movie_id", 10, "person_role_id", 1, "role_id", 1, "note", "Soldier (voice) (uncredited)")), new java.util.HashMap<>(java.util.Map.of("movie_id", 11, "person_role_id", 2, "role_id", 1, "note", "(voice)"))};
    
    static Object[] company_name = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1, "country_code", "[ru]")), new java.util.HashMap<>(java.util.Map.of("id", 2, "country_code", "[us]"))};
    
    static Object[] company_type = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1)), new java.util.HashMap<>(java.util.Map.of("id", 2))};
    
    static Object[] movie_companies = new Object[]{new java.util.HashMap<>(java.util.Map.of("movie_id", 10, "company_id", 1, "company_type_id", 1)), new java.util.HashMap<>(java.util.Map.of("movie_id", 11, "company_id", 2, "company_type_id", 1))};
    
    static Object[] role_type = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1, "role", "actor")), new java.util.HashMap<>(java.util.Map.of("id", 2, "role", "director"))};
    
    static Object[] title = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 10, "title", "Vodka Dreams", "production_year", 2006)), new java.util.HashMap<>(java.util.Map.of("id", 11, "title", "Other Film", "production_year", 2004))};
    
    static Object[] matches = (new java.util.function.Supplier<java.util.List<Object>>() {
    public java.util.List<Object> get() {
        java.util.List<Object> _src = _toList(char_name);
        java.util.List<_JoinSpec> _joins = java.util.List.of(
            new _JoinSpec(_toList(cast_info), (Object[] a) -> { Object chn = a[0]; Object ci = a[1]; return (chn.get("id") == ci.get("person_role_id")); }, false, false),
            new _JoinSpec(_toList(role_type), (Object[] a) -> { Object chn = a[0]; Object ci = a[1]; Object rt = a[2]; return (rt.get("id") == ci.get("role_id")); }, false, false),
            new _JoinSpec(_toList(title), (Object[] a) -> { Object chn = a[0]; Object ci = a[1]; Object rt = a[2]; Object t = a[3]; return (t.get("id") == ci.get("movie_id")); }, false, false),
            new _JoinSpec(_toList(movie_companies), (Object[] a) -> { Object chn = a[0]; Object ci = a[1]; Object rt = a[2]; Object t = a[3]; Object mc = a[4]; return (mc.get("movie_id") == t.get("id")); }, false, false),
            new _JoinSpec(_toList(company_name), (Object[] a) -> { Object chn = a[0]; Object ci = a[1]; Object rt = a[2]; Object t = a[3]; Object mc = a[4]; Object cn = a[5]; return (cn.get("id") == mc.get("company_id")); }, false, false),
            new _JoinSpec(_toList(company_type), (Object[] a) -> { Object chn = a[0]; Object ci = a[1]; Object rt = a[2]; Object t = a[3]; Object mc = a[4]; Object cn = a[5]; Object ct = a[6]; return (ct.get("id") == mc.get("company_type_id")); }, false, false)
        );
        return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object chn = a[0]; Object ci = a[1]; Object rt = a[2]; Object t = a[3]; Object mc = a[4]; Object cn = a[5]; Object ct = a[6]; return new java.util.HashMap<>(java.util.Map.of("character", chn.get("name"), "movie", t.get("title"))); }, (Object[] a) -> { Object chn = a[0]; Object ci = a[1]; Object rt = a[2]; Object t = a[3]; Object mc = a[4]; Object cn = a[5]; Object ct = a[6]; return ((((ci.get("note").contains("(voice)") && ci.get("note").contains("(uncredited)")) && (cn.get("country_code") == "[ru]")) && (rt.get("role") == "actor")) && (t.get("production_year") > 2005)); }, null, -1, -1));
    }
}).get();
    
    static Object[] result = new Object[]{new java.util.HashMap<>(java.util.Map.of("uncredited_voiced_character", min.apply((new java.util.function.Supplier<java.util.List<Object>>() {
    public java.util.List<Object> get() {
        java.util.List<Object> _src = _toList(matches);
        java.util.List<_JoinSpec> _joins = java.util.List.of(
        );
        return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object x = a[0]; return x.get("character"); }, null, null, -1, -1));
    }
}).get()), "russian_movie", min.apply((new java.util.function.Supplier<java.util.List<Object>>() {
    public java.util.List<Object> get() {
        java.util.List<Object> _src = _toList(matches);
        java.util.List<_JoinSpec> _joins = java.util.List.of(
        );
        return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object x = a[0]; return x.get("movie"); }, null, null, -1, -1));
    }
}).get())))};
    
    public static void main(String[] args) {
        test_Q10_finds_uncredited_voice_actor_in_Russian_movie();
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
