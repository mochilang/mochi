import java.util.*;
class Todo {
	String title;
	Todo(String title) {
		this.title = title;
	}
}
public class Main {
	static Map<String,String> todo = new HashMap<>(new Todo("hi"));
	public static void main(String[] args) {
	System.out.println(todo.title);
	}
}
