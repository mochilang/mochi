import java.util.*;
public class Main {
	public static void main(String[] args) {
	System.out.println(new LinkedHashMap<String,Integer>(){{put("a", 1);put("b", 2);}}.size());
	}
}
