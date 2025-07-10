import java.util.*;
class DataClass1 {
	int a;
	int b;
	DataClass1(int a, int b) {
		this.a = a;
		this.b = b;
	}
}
public class Main {
	static DataClass1 m = new DataClass1(1, 2);
	public static void main(String[] args) {
	System.out.println(m.get("b"));
	}
}
