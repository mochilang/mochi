import java.util.*;
import java.io.*;
class Person {
	String name;
	int age;
	String email;
	Person(String name, int age, String email) {
		this.name = name;
		this.age = age;
		this.email = email;
	}
}
public class Main {
	static List<Object> people = loadYaml("../interpreter/valid/people.yaml");
	static List<Object> adults = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var p : people) {
		if (!(Boolean.TRUE.equals(((Number)((Map)p).get("age")).doubleValue() >= 18))) continue;
		_res1.add(new LinkedHashMap<>(){{put("name", ((Map)p).get("name"));put("email", ((Map)p).get("email"));}});
	}
	return _res1;
}}).get();
	static List<Map<String,Object>> loadYaml(String path) throws Exception {
		List<Map<String,Object>> list = new ArrayList<>();
		try (BufferedReader br = new BufferedReader(new FileReader(path))) {
			Map<String,Object> cur = null;
			String line;
			while ((line = br.readLine()) != null) {
				line = line.trim();
				if (line.startsWith("- name:")) {
					if (cur != null) list.add(cur);
					cur = new LinkedHashMap<>();
					cur.put("name", line.substring(line.indexOf(':')+1).trim());
				} else if (line.startsWith("age:")) {
					if (cur != null) cur.put("age", Integer.parseInt(line.substring(line.indexOf(':')+1).trim()));
				} else if (line.startsWith("email:")) {
					if (cur != null) cur.put("email", line.substring(line.indexOf(':')+1).trim());
				}
			}
			if (cur != null) list.add(cur);
		}
		return list;
	}
	public static void main(String[] args) {
	for (var a : adults) {
		System.out.println(((Map)a).get("name") + " " + ((Map)a).get("email"));
	}
	}
}
