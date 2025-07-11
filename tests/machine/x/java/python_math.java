class math {
	static double pi = Math.PI;
	static double e = Math.E;
	static double sqrt(double x) { return Math.sqrt(x); }
	static double pow(double x, double y) { return Math.pow(x, y); }
	static double sin(double x) { return Math.sin(x); }
	static double log(double x) { return Math.log(x); }
}
public class PythonMath {
	static double r = 3.000000;
	static double area = math.pi * math.pow(r, 2.000000);
	static Object root = math.sqrt(49.000000);
	static Object sin45 = math.sin(math.pi / 4.000000);
	static Object log_e = math.log(math.e);
	public static void main(String[] args) {
	System.out.println("Circle area with r =" + " " + r + " " + "=>" + " " + area);
	System.out.println("Square root of 49:" + " " + root);
	System.out.println("sin(π/4):" + " " + sin45);
	System.out.println("log(e):" + " " + log_e);
	}
}
