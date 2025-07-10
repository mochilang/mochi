import java.util.*;
class DataClass1 {
	int inner;
	DataClass1(int inner) {
		this.inner = inner;
	}
}
class DataClass1 {
	DataClass1 outer;
	DataClass1(DataClass1 outer) {
		this.outer = outer;
	}
}
public class Main {
	static Map<String,Map<String,Integer>> data = mapOfEntries(entry("outer", mapOfEntries(entry("inner", 1))));
	static <K,V> Map.Entry<K,V> entry(K k, V v) { return new AbstractMap.SimpleEntry<>(k, v); }
	static <K,V> LinkedHashMap<K,V> mapOfEntries(Map.Entry<? extends K,? extends V>... entries) {
		LinkedHashMap<K,V> m = new LinkedHashMap<>();
		for (var e : entries) m.put(e.getKey(), e.getValue());
		return m;
	}
	public static void main(String[] args) {
	((List)data.get("outer")).put("inner", 2);
	System.out.println(((Map)data.get("outer")).get("inner"));
	}
}
