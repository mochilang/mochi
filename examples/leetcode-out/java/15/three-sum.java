public class Main {
	static Object[] threeSum(Object[] nums) {
		Object[] sorted = (new java.util.function.Supplier<java.util.List<Object>>() {
	public java.util.List<Object> get() {
		java.util.List<Object> _src = _toList(nums);
		java.util.List<_JoinSpec> _joins = java.util.List.of(
		);
		return _query(_src, _joins, new _QueryOpts((Object[] a) -> { Object x = a[0]; return x; }, null, (Object[] a) -> { Object x = a[0]; return x; }, -1, -1));
	}
}).get();
		int n = sorted.length;
		Object[] res = new Object[][]{};
		int i = 0;
		while ((i < n)) {
			if (((i > 0) && (sorted[i] == sorted[(i - 1)]))) {
				i = (i + 1);
				continue;
			}
			int left = (i + 1);
			int right = (n - 1);
			while ((left < right)) {
				int sum = ((sorted[i] + sorted[left]) + sorted[right]);
				if ((sum == 0)) {
					res = _concat(res, new Object[][]{new int[]{sorted[i], sorted[left], sorted[right]}});
					left = (left + 1);
					while (((left < right) && (sorted[left] == sorted[(left - 1)]))) {
						left = (left + 1);
					}
					right = (right - 1);
					while (((left < right) && (sorted[right] == sorted[(right + 1)]))) {
						right = (right - 1);
					}
				} else 				if ((sum < 0)) {
					left = (left + 1);
				} else {
					right = (right - 1);
				}
			}
			i = (i + 1);
		}
		return res;
	}
	
	static void test_example_1() {
		expect((threeSum(new int[]{(-1), 0, 1, 2, (-1), (-4)}) == new Object[][]{new int[]{(-1), (-1), 2}, new int[]{(-1), 0, 1}}));
	}
	
	static void test_example_2() {
		expect((threeSum(new int[]{0, 1, 1}) == new Object[]{}));
	}
	
	static void test_example_3() {
		expect((threeSum(new int[]{0, 0, 0}) == new Object[][]{new int[]{0, 0, 0}}));
	}
	
	public static void main(String[] args) {
		test_example_1();
		test_example_2();
		test_example_3();
	}
	
	static int[] _concat(int[] a, int[] b) {
		int[] res = new int[a.length + b.length];
		System.arraycopy(a, 0, res, 0, a.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
	
	static double[] _concat(double[] a, double[] b) {
		double[] res = new double[a.length + b.length];
		System.arraycopy(a, 0, res, 0, a.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
	
	static boolean[] _concat(boolean[] a, boolean[] b) {
		boolean[] res = new boolean[a.length + b.length];
		System.arraycopy(a, 0, res, 0, a.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
	}
	
	static <T> T[] _concat(T[] a, T[] b) {
		T[] res = java.util.Arrays.copyOf(a, a.length + b.length);
		System.arraycopy(b, 0, res, a.length, b.length);
		return res;
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
