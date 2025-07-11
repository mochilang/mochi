import java.util.*;
import java.util.function.*;
public class MatchFull {
	static int x = 2;
	static Object label = (new java.util.function.Supplier<String>(){public String get(){
	var _t3 = x;
	if (Objects.equals(_t3, 1)) return "one";
	else if (Objects.equals(_t3, 2)) return "two";
	else if (Objects.equals(_t3, 3)) return "three";
	return "unknown";
}}).get();
	static String day = "sun";
	static Object mood = (new java.util.function.Supplier<String>(){public String get(){
	var _t4 = day;
	if (Objects.equals(_t4, "mon")) return "tired";
	else if (Objects.equals(_t4, "fri")) return "excited";
	else if (Objects.equals(_t4, "sun")) return "relaxed";
	return "normal";
}}).get();
	static boolean ok = true;
	static Object status = (new java.util.function.Supplier<String>(){public String get(){
	var _t5 = ok;
	if (Objects.equals(_t5, true)) return "confirmed";
	else if (Objects.equals(_t5, false)) return "denied";
	return null;
}}).get();
	static String classify(int n) {
		return (new java.util.function.Supplier<String>(){public String get(){
	var _t6 = n;
	if (Objects.equals(_t6, 0)) return "zero";
	else if (Objects.equals(_t6, 1)) return "one";
	return "many";
}}).get();
	}
	public static void main(String[] args) {
	System.out.println(label);
	System.out.println(mood);
	System.out.println(status);
	System.out.println(classify(0));
	System.out.println(classify(5));
	}
}
