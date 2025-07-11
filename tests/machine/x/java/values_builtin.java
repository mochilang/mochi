import java.util.*;
class ABC {
	int a;
	int b;
	int c;
	ABC(int a, int b, int c) {
		this.a = a;
		this.b = b;
		this.c = c;
	}
	int size() { return 3; }
}
public class ValuesBuiltin {
	public static void main(String[] args) {
	ABC m = new ABC(1, 2, 3);
	System.out.println(Arrays.asList(m.a, m.b, m.c));
	}
}
