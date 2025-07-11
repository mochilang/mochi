import java.util.*;
public class StringPrefixSlice {
	public static void main(String[] args) {
	String prefix = "fore";
	String s1 = "forest";
	System.out.println(Objects.equals(s1.substring(0, prefix.length()), prefix));
	String s2 = "desert";
	System.out.println(Objects.equals(s2.substring(0, prefix.length()), prefix));
	}
}
