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
	@Override public boolean equals(Object o) {
		if (this == o) return true;
		if (!(o instanceof Person other)) return false;
		return Objects.equals(this.name, other.name) && Objects.equals(this.age, other.age) && Objects.equals(this.email, other.email);
	}
	@Override public int hashCode() {
		return Objects.hash(name, age, email);
	}
}
public class Main {
	static List<Map<String,Object>> people = loadYaml("../interpreter/valid/people.yaml");
	static List<Object> adults = (new java.util.function.Supplier<List<Object>>() {public List<Object> get() {
	List<Object> _res1 = new ArrayList<>();
	for (var p : people) {
		if (!(((Number)((Map)p).get("age")).doubleValue() >= 18)) continue;
		_res1.add(new LinkedHashMap<String,Object>(){{put("name", ((Map)p).get("name"));put("email", ((Map)p).get("email"));}});
	}
	return _res1;
}}).get();
	static List<Map<String,Object>> loadYaml(String path) {
		if (!(new java.io.File(path)).isAbsolute()) {
			java.io.File f = new java.io.File(path);
			if (!f.exists()) {
				String root = System.getenv("MOCHI_ROOT");
				if (root != null && !root.isEmpty()) {
					String clean = path;
					while (clean.startsWith("../")) clean = clean.substring(3);
					java.io.File alt = new java.io.File(root + java.io.File.separator + "tests" + java.io.File.separator + clean);
					if (!alt.exists()) alt = new java.io.File(root, clean);
					if (alt.exists()) path = alt.getPath();
				}
			}
		}
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
		} catch (Exception e) { throw new RuntimeException(e); }
		return list;
	}
	public static void main(String[] args) {
	for (var a : adults) {
		System.out.println(((Map)a).get("name") + " " + ((Map)a).get("email"));
	}
	}
}
