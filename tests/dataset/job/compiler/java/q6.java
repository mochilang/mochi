public class Main {
    static void test_Q6_finds_marvel_movie_with_Robert_Downey() {
        expect((result == new Object[]{new java.util.HashMap<>(java.util.Map.of("movie_keyword", "marvel-cinematic-universe", "actor_name", "Downey Robert Jr.", "marvel_movie", "Iron Man 3"))}));
    }
    
    static Object[] cast_info = new Object[]{new java.util.HashMap<>(java.util.Map.of("movie_id", 1, "person_id", 101)), new java.util.HashMap<>(java.util.Map.of("movie_id", 2, "person_id", 102))};
    
    static Object[] keyword = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 100, "keyword", "marvel-cinematic-universe")), new java.util.HashMap<>(java.util.Map.of("id", 200, "keyword", "other"))};
    
    static Object[] movie_keyword = new Object[]{new java.util.HashMap<>(java.util.Map.of("movie_id", 1, "keyword_id", 100)), new java.util.HashMap<>(java.util.Map.of("movie_id", 2, "keyword_id", 200))};
    
    static Object[] name = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 101, "name", "Downey Robert Jr.")), new java.util.HashMap<>(java.util.Map.of("id", 102, "name", "Chris Evans"))};
    
    static Object[] title = new Object[]{new java.util.HashMap<>(java.util.Map.of("id", 1, "title", "Iron Man 3", "production_year", 2013)), new java.util.HashMap<>(java.util.Map.of("id", 2, "title", "Old Movie", "production_year", 2000))};
    
    static Object[] result = (new java.util.function.Supplier<java.util.List<Object>>() {
    public java.util.List<Object> get() {
        java.util.List<Object> _src = _toList(cast_info);
        java.util.List<_JoinSpec> _joins = java.util.List.of(
            new _JoinSpec(_toList(movie_keyword), (Object[] a) -> { Object ci = a[0]; Object mk = a[1]; return (ci.get("movie_id") == mk.get("movie_id")); }, false, false),
            new _JoinSpec(_toList(keyword), (Object[] a) -> { Object ci = a[0]; Object mk = a[1]; Object k = a[2]; return (mk.get("keyword_id") == k.get("id")); }, false, false),
            new _JoinSpec(_toList(name), (Object[] a) -> { Object ci = a[0]; Object mk = a[1]; Object k = a[2]; Object n = a[3]; return (ci.get("person_id") == n.get("id")); }, false, false),
            new _JoinSpec(_toList(title), (Object[] a) -> { Object ci = a[0]; Object mk = a[1]; Object k = a[2]; Object n = a[3]; Object t = a[4]; return (ci.get("movie_id") == t.get("id")); }, false, false)
        );
        return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object ci = a[0]; Object mk = a[1]; Object k = a[2]; Object n = a[3]; Object t = a[4]; return new java.util.HashMap<>(java.util.Map.of("movie_keyword", k.get("keyword"), "actor_name", n.get("name"), "marvel_movie", t.get("title"))); }, (Object[] a) -> { Object ci = a[0]; Object mk = a[1]; Object k = a[2]; Object n = a[3]; Object t = a[4]; return ((((k.get("keyword") == "marvel-cinematic-universe") && n.get("name").contains("Downey")) && n.get("name").contains("Robert")) && (t.get("production_year") > 2010)); }, null, -1, -1));
    }
}).get();
    
    public static void main(String[] args) {
        test_Q6_finds_marvel_movie_with_Robert_Downey();
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
