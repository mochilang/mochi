import java.util.*;
class Todo {
	String title;
	Todo(String title) {
		this.title = title;
	}
}
public class Main {
	static Todo todo = new Todo("hi");
	public static void main(String[] args) {
	System.out.println(todo.title);
	}
}
