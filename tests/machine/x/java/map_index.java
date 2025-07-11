class AB {
	int a;
	int b;
	AB(int a, int b) {
		this.a = a;
		this.b = b;
	}
	int size() { return 2; }
}
public class MapIndex {
	public static void main(String[] args) {
	AB m = new AB(1, 2);
	System.out.println(m.b);
	}
}
