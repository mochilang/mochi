import java.util.*;
class DataClass1 {
	Object a;
	Object b;
	DataClass1(Object a, Object b) {
		this.a = a;
		this.b = b;
	}
}
public class Main {
	static int x = 3;
	static int y = 4;
	static DataClass1 m = new DataClass1(x, y);
	public static void main(String[] args) {
	System.out.println(m.get("a") + " " + m.get("b"));
	}
}
