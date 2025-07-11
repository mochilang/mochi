import java.util.*;
public class IfThenElseNested {
	static int x = 8;
	static Object msg = (x > 10 ? "big" : (x > 5 ? "medium" : "small"));
	public static void main(String[] args) {
	System.out.println(msg);
	}
}
