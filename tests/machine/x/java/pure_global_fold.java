import java.util.*;
public class PureGlobalFold {
	static int k = 2;
	static int inc(int x) {
		return x + k;
	}
	public static void main(String[] args) {
	System.out.println(inc(3));
	}
}
