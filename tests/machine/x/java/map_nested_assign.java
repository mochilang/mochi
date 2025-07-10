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
	static DataClass1 data = new DataClass1(new DataClass1(1));
	public static void main(String[] args) {
	((List)data.get("outer")).put("inner", 2);
	System.out.println(((Map)data.get("outer")).get("inner"));
	}
}
