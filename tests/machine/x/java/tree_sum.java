import java.util.*;
class Tree {
	static class Leaf extends Tree {}
	static class Node extends Tree {
		Tree left;
		int value;
		Tree right;
		Node(Tree left, int value, Tree right) {
			this.left = left;
			this.value = value;
			this.right = right;
		}
	}
}
public class Main {
	static Tree t = new Tree.Node(new Tree.Leaf(), 1, new Tree.Node(new Tree.Leaf(), 2, new Tree.Leaf()));
	static int sum_tree(Tree t) {
		return (new java.util.function.Supplier<Integer>(){public Integer get(){
	var _t0 = t;
	if (_t0 instanceof Tree.Leaf) return 0;
	if (_t0 instanceof Tree.Node _v1) {
		var left = _v1.left;
		var value = _v1.value;
		var right = _v1.right;
		return sum_tree(left) + value + sum_tree(right);
	}
	return null;
}}).get();
	}
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(sum_tree(t));
	}
}
