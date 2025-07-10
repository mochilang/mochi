import java.util.*;
class Person {
	String name;
	int age;
	Person(String name, int age) {
		this.name = name;
		this.age = age;
	}
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof Person other)) return false;
		return Objects.equals(this.name, other.name) && Objects.equals(this.age, other.age);
	}
	@Override public int hashCode() {
		return Objects.hash(name, age);
	}
}
class Book {
	String title;
	Person author;
	Book(String title, Person author) {
		this.title = title;
		this.author = author;
	}
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof Book other)) return false;
		return Objects.equals(this.title, other.title) && Objects.equals(this.author, other.author);
	}
	@Override public int hashCode() {
		return Objects.hash(title, author);
	}
}
public class Main {
	static Book book = new Book("Go", new Person("Bob", 42));
	static <K,V> LinkedHashMap<K,V> mapOf(Object... kv) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (int i = 0; i < kv.length; i += 2) m.put((K)kv[i], (V)kv[i+1]);
		return m;
	}
	public static void main(String[] args) {
	System.out.println(book.author.name);
	}
}
