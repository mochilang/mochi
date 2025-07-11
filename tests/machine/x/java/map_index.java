class MAB {
	int a;
	int b;
	MAB(int a, int b) {
		this.a = a;
		this.b = b;
	}
}
public class MapIndex {
	static MAB m = new MAB(1, 2);
	public static void main(String[] args) {
	System.out.println(m.b);
	}
}
