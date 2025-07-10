import java.util.*;
class DataClass1 {
	int a;
	int b;
	DataClass1(int a, int b) {
		this.a = a;
		this.b = b;
	}
}
public class Main {
	public static void main(String[] args) {
	System.out.println(new DataClass1(1, 2).size());
	}
}
