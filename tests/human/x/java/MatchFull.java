public class MatchFull {
    static String classify(int n) {
        switch (n) {
            case 0: return "zero";
            case 1: return "one";
            default: return "many";
        }
    }

    public static void main(String[] args) {
        int x = 2;
        String label;
        switch (x) {
            case 1: label = "one"; break;
            case 2: label = "two"; break;
            case 3: label = "three"; break;
            default: label = "unknown"; break;
        }
        System.out.println(label);

        String day = "sun";
        String mood;
        switch (day) {
            case "mon": mood = "tired"; break;
            case "fri": mood = "excited"; break;
            case "sun": mood = "relaxed"; break;
            default: mood = "normal"; break;
        }
        System.out.println(mood);

        boolean ok = true;
        String status = ok ? "confirmed" : "denied";
        System.out.println(status);

        System.out.println(classify(0));
        System.out.println(classify(5));
    }
}
