import java.util.*;
public class Main {
	static String prefix = "fore";
	static String s1 = "forest";
	static String s2 = "desert";
	public static void main(String[] args) {
	System.out.println(Objects.equals(s1.substring(0, prefix.length()), prefix));
	System.out.println(Objects.equals(s2.substring(0, prefix.length()), prefix));
	}
}
