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
	static <K,V> List<V> values(Map<K,V> m) {
		return new ArrayList<>(m.values());
	}
	public static void main(String[] args) {
	System.out.println(values(m));
	}
}
