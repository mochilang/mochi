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
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	System.out.println(todo.title);
	}
}
