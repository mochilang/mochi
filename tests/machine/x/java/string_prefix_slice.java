import java.util.*;
public class Main {
	static String prefix = "fore";
	static String s1 = "forest";
	static String s2 = "desert";
	static String sliceString(String s, int i, int j) {
		int start = i;
		int end = j;
		int n = s.length();
		if (start < 0) start += n;
		if (end < 0) end += n;
		if (start < 0) start = 0;
		if (end > n) end = n;
		if (end < start) end = start;
		return s.substring(start, end);
	}
	public static void main(String[] args) {
	System.out.println(sliceString(s1, 0, prefix.length()) == prefix);
	System.out.println(sliceString(s2, 0, prefix.length()) == prefix);
	}
}
