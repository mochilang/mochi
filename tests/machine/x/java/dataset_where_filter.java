import java.util.*;
class PeopleAgeName {
	String name;
	int age;
	PeopleAgeName(String name, int age) {
		this.name = name;
		this.age = age;
	}
}
class AdultsAgeIsSeniorName {
	String name;
	int age;
	boolean is_senior;
	AdultsAgeIsSeniorName(String name, int age, boolean is_senior) {
		this.name = name;
		this.age = age;
		this.is_senior = is_senior;
	}
}
public class Main {
	static List<PeopleAgeName> people = new ArrayList<>(Arrays.asList(new PeopleAgeName("Alice", 30), new PeopleAgeName("Bob", 15), new PeopleAgeName("Charlie", 65), new PeopleAgeName("Diana", 45)));
	static List<AdultsAgeIsSeniorName> adults = (new java.util.function.Supplier<List<AdultsAgeIsSeniorName>>(){public List<AdultsAgeIsSeniorName> get(){
	List<AdultsAgeIsSeniorName> _res1 = new ArrayList<>();
	for (var person : people) {
		if (!(person.age >= 18)) continue;
		_res1.add(new AdultsAgeIsSeniorName(person.name, person.age, person.age >= 60));
	}
	return _res1;
}}).get();
	public static void main(String[] args) {
	System.out.println("--- Adults ---");
	for (AdultsAgeIsSeniorName person : adults) {
		System.out.println(person.name + " " + "is" + " " + person.age + " " + (person.is_senior ? " (senior)" : ""));
	}
	}
}
