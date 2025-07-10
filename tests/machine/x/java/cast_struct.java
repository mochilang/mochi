import java.util.*;
class Todo {
	String title;
	Todo(String title) {
		this.title = title;
	}
}
public class Main {
	static Todo todo = new Todo("hi");
	static Map<Object,Object> map(Object... kv) {
		Map<Object,Object> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put(String.valueOf(kv[i]), kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(todo.title);
	}
}
