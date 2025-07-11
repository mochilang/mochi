import java.util.*;
class MABC {
	int a;
	int b;
	int c;
	MABC(int a, int b, int c) {
		this.a = a;
		this.b = b;
		this.c = c;
	}
}
public class ValuesBuiltin {
	static MABC m = new MABC(1, 2, 3);
	public static void main(String[] args) {
	System.out.println(Arrays.asList(m.a, m.b, m.c));
	}
}
