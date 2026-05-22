public class Main {
    static int x = 2;
    static String label = x == 1 ? "one" : x == 2 ? "two" : x == 3 ? "three" : "unknown";
    static String day = "sun";
    static String mood = (day.equals("mon")) ? "tired" : (day.equals("fri")) ? "excited" : (day.equals("sun")) ? "relaxed" : "normal";
    static boolean ok = true;
    static String status = ok == true ? "confirmed" : ok == false ? "denied" : "denied";

    static String classify(int n) {
        return n == 0 ? "zero" : n == 1 ? "one" : "many";
    }
    public static void main(String[] args) {
        System.out.println(label);
        System.out.println(mood);
        System.out.println(status);
        System.out.println(classify(0));
        System.out.println(classify(5));
    }
}
