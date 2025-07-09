import java.util.*;
public class Main {
	static <T> List<T> slice(List<T> obj, int i, int j) {
		int start = i;
		int end = j;
		int n = obj.size();
		if (start < 0) start += n;
		if (end < 0) end += n;
		if (start < 0) start = 0;
		if (end > n) end = n;
		if (end < start) end = start;
		return new ArrayList<>(obj.subList(start, end));
	}
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
	System.out.println(slice(java.util.Arrays.asList(1, 2, 3), 1, 3));
	System.out.println(slice(java.util.Arrays.asList(1, 2, 3), 0, 2));
	System.out.println(sliceString("hello", 1, 4));
	}
}
