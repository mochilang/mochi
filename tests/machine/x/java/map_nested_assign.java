import java.util.*;
public class Main {
	static int data = java.util.Map.of("outer", java.util.Map.of("inner", 1));
	public static void main(String[] args) {
	data = 2;
	System.out.println(data.get("outer").get("inner"));
	}
}
