import java.util.*;
public class Main {
	static int x = 2;
	static Object label = (new java.util.function.Supplier<String>(){public String get(){
	var _t0 = x;
	if (Objects.equals(_t0, 1)) return "one";
	else if (Objects.equals(_t0, 2)) return "two";
	else if (Objects.equals(_t0, 3)) return "three";
	return "unknown";
}}).get();
	public static void main(String[] args) {
	System.out.println(label);
	}
}
