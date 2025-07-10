import java.util.*;
class DataClass1 {
	int alice;
	DataClass1(int alice) {
		this.alice = alice;
	}
}
public class Main {
	static DataClass1 scores = new DataClass1(1);
	public static void main(String[] args) {
	scores.put("bob", 2);
	System.out.println(scores.get("bob"));
	}
}
