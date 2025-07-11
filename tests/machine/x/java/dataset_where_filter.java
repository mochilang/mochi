import java.util.*;
class NameAge {
	String name;
	int age;
	NameAge(String name, int age) {
		this.name = name;
		this.age = age;
	}
	int size() { return 2; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof NameAge other)) return false;
		return Objects.equals(this.name, other.name) && Objects.equals(this.age, other.age);
	}
	@Override public int hashCode() {
		return Objects.hash(name, age);
	}
}
class NameAgeIsSenior {
	String name;
	int age;
	boolean is_senior;
	NameAgeIsSenior(String name, int age, boolean is_senior) {
		this.name = name;
		this.age = age;
		this.is_senior = is_senior;
	}
	int size() { return 3; }
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof NameAgeIsSenior other)) return false;
		return Objects.equals(this.name, other.name) && Objects.equals(this.age, other.age) && Objects.equals(this.is_senior, other.is_senior);
	}
	@Override public int hashCode() {
		return Objects.hash(name, age, is_senior);
	}
}
public class DatasetWhereFilter {
	public static void main(String[] args) {
	List<NameAge> people = new ArrayList<>(Arrays.asList(new NameAge("Alice", 30), new NameAge("Bob", 15), new NameAge("Charlie", 65), new NameAge("Diana", 45)));
	List<NameAgeIsSenior> adults = (new java.util.function.Supplier<List<NameAgeIsSenior>>(){public List<NameAgeIsSenior> get(){
	List<NameAgeIsSenior> _res0 = new ArrayList<>();
	for (var person : people) {
		if (!(person.age >= 18)) continue;
		_res0.add(new NameAgeIsSenior(person.name, person.age, person.age >= 60));
	}
	return _res0;
}}).get();
	System.out.println("--- Adults ---");
	for (NameAgeIsSenior person : adults) {
		System.out.println(person.name + " " + "is" + " " + person.age + " " + (person.is_senior ? " (senior)" : ""));
	}
	}
}
