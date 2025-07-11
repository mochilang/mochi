class Inner {
	int inner;
	Inner(int inner) {
		this.inner = inner;
	}
	int size() { return 1; }
}
class Outer {
	Inner outer;
	Outer(Inner outer) {
		this.outer = outer;
	}
	int size() { return 1; }
}
public class MapNestedAssign {
	public static void main(String[] args) {
	Outer data = new Outer(new Inner(1));
	data.outer.inner = 2;
	System.out.println(data.outer.inner);
	}
}
