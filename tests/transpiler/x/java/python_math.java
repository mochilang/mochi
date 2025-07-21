public class Main {
    static double r = 3.0;
    static double area = Math.PI * Math.pow(r, 2.0);
    static double root = Math.sqrt(49.0);
    static double sin45 = Math.sin(Math.PI / 4.0);
    static double log_e = Math.log(Math.E);

    public static void main(String[] args) {
        System.out.println("Circle area with r =" + " " + r + " " + "=>" + " " + area);
        System.out.println("Square root of 49:" + " " + root);
        System.out.println("sin(π/4):" + " " + sin45);
        System.out.println("log(e):" + " " + log_e);
    }
}
