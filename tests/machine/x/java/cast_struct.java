import java.util.*;
class Todo {
	String title;
	Todo(String title) {
		this.title = title;
	}
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof Todo other)) return false;
		return Objects.equals(this.title, other.title);
	}
	@Override public int hashCode() {
		return Objects.hash(title);
	}
}
public class Main {
	static Todo todo = new Todo("hi");
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(todo.title);
	}
}
