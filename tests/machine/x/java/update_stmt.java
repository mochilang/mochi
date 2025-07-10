import java.util.*;
class Person {
	String name;
	int age;
	String status;
	Person(String name, int age, String status) {
		this.name = name;
		this.age = age;
		this.status = status;
	}
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof Person other)) return false;
		return Objects.equals(this.name, other.name) && Objects.equals(this.age, other.age) && Objects.equals(this.status, other.status);
	}
	@Override public int hashCode() {
		return Objects.hash(name, age, status);
	}
}
public class Main {
	static List<Person> people = java.util.Arrays.asList(new Person("Alice", 17, "minor"), new Person("Bob", 25, "unknown"), new Person("Charlie", 18, "unknown"), new Person("Diana", 16, "minor"));
	public static void main(String[] args) {
	for (Person _it0 : people) {
		if (!(_it0.age >= 18)) continue;
		_it0.status = "adult";
		_it0.age = _it0.age + 1;
	}
	if (!(Objects.equals(people, java.util.Arrays.asList(new Person("Alice", 17, "minor"), new Person("Bob", 26, "adult"), new Person("Charlie", 19, "adult"), new Person("Diana", 16, "minor"))))) throw new AssertionError("expect failed");
	System.out.println("ok");
	}
}
