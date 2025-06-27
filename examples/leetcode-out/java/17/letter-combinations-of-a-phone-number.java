public class Main {
	static Object[] letterCombinations(String digits) {
		if ((digits.length() == 0)) {
			return new String[]{};
		}
		java.util.Map<String, Object[]> mapping = new java.util.HashMap<>(java.util.Map.of("2", new String[]{"a", "b", "c"}, "3", new String[]{"d", "e", "f"}, "4", new String[]{"g", "h", "i"}, "5", new String[]{"j", "k", "l"}, "6", new String[]{"m", "n", "o"}, "7", new String[]{"p", "q", "r", "s"}, "8", new String[]{"t", "u", "v"}, "9", new String[]{"w", "x", "y", "z"}));
		Object[] result = new String[]{""};
		for (var d : digits.toCharArray()) {
			if ((!_in(d, mapping))) {
				continue;
			}
			Object[] letters = mapping.get(d);
			Object[] next = (new java.util.function.Supplier<java.util.List<Object>>() {
	public java.util.List<Object> get() {
		java.util.List<Object> _src = _toList(result);
		java.util.List<_JoinSpec> _joins = java.util.List.of(
			new _JoinSpec(_toList(letters), null, false, false)
		);
		return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object p = a[0]; Object ch = a[1]; return (p + ch); }, null, null, -1, -1));
	}
}).get();
			result = next;
		}
		return result;
	}
	
	static void test_example_1() {
		expect((letterCombinations("23") == new String[]{"ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"}));
	}
	
	static void test_example_2() {
		expect((letterCombinations("") == new Object[]{}));
	}
	
	static void test_example_3() {
		expect((letterCombinations("2") == new String[]{"a", "b", "c"}));
	}
	
	static void test_single_seven() {
		expect((letterCombinations("7") == new String[]{"p", "q", "r", "s"}));
	}
	
	static void test_mix() {
		expect((letterCombinations("79") == new String[]{"pw", "px", "py", "pz", "qw", "qx", "qy", "qz", "rw", "rx", "ry", "rz", "sw", "sx", "sy", "sz"}));
	}
	
	public static void main(String[] args) {
		test_example_1();
		test_example_2();
		test_example_3();
		test_single_seven();
		test_mix();
	}
	
	static boolean _in(Object item, Object col) {
		if (col instanceof String s && item instanceof String sub) return s.contains(sub);
		if (col instanceof java.util.Map<?,?> m) return m.containsKey(item);
		if (col != null && col.getClass().isArray()) {
			int n = java.lang.reflect.Array.getLength(col);
			for (int i = 0; i < n; i++) {
				if (java.util.Objects.equals(java.lang.reflect.Array.get(col, i), item)) return true;
			}
			return false;
		}
		if (col instanceof Iterable<?> it) {
			for (Object v : it) {
				if (java.util.Objects.equals(v, item)) return true;
			}
			return false;
		}
		return false;
	}
	
	static void expect(boolean cond) {
		if (!cond) throw new RuntimeException("expect failed");
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
