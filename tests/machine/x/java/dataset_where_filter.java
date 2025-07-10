import java.util.*;
class DataClass1 {
	String name;
	int age;
	DataClass1(String name, int age) {
		this.name = name;
		this.age = age;
	}
}
class DataClass2 {
	Object name;
	Object age;
	Object is_senior;
	DataClass2(Object name, Object age, Object is_senior) {
		this.name = name;
		this.age = age;
		this.is_senior = is_senior;
	}
}
public class Main {
	static List<DataClass1> people = new ArrayList<>(java.util.Arrays.asList(new DataClass1("Alice", 30), new DataClass1("Bob", 15), new DataClass1("Charlie", 65), new DataClass1("Diana", 45)));
	static List<DataClass2> adults = (new java.util.function.Supplier<List<DataClass2>>(){public List<DataClass2> get(){
	List<DataClass2> _res1 = new ArrayList<>();
	for (var person : people) {
		if (!(person.age >= 18)) continue;
		_res1.add(new DataClass2(person.name, person.age, person.age >= 60));
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Adults ---");
	for (Object person : adults) {
		System.out.println(((Map)person).get("name") + " " + "is" + " " + ((Map)person).get("age") + " " + (((Map)person).get("is_senior") != null ? " (senior)" : ""));
	}
	}
}
