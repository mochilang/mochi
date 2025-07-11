class MAB {
	int a;
	int b;
	MAB(int a, int b) {
		this.a = a;
		this.b = b;
	}
}
public class MapLiteralDynamic {
	static int x = 3;
	static int y = 4;
	static MAB m = new MAB(x, y);
	public static void main(String[] args) {
	System.out.println(m.a + " " + m.b);
	}
}
