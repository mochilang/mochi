import java.util.*;
class DataClass1 {
	int a;
	int b;
	int c;
	DataClass1(int a, int b, int c) {
		this.a = a;
		this.b = b;
		this.c = c;
	}
}
public class Main {
	static DataClass1 m = new DataClass1(1, 2, 3);
	static <K,V> List<V> values(Map<K,V> m) {
		return new ArrayList<>(m.values());
	}
	public static void main(String[] args) {
	System.out.println(values(m));
	}
}
