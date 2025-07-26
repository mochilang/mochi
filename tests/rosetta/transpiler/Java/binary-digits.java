public class Main {

    static String toBin(int n) {
        if (n == 0) {
            return "0";
        }
        String bits = "";
        int x = n;
        while (x > 0) {
            bits = String.valueOf(Math.floorMod(x, 2)) + bits;
            x = ((Number)((x / 2))).intValue();
        }
        return bits;
    }
    public static void main(String[] args) {
        for (int i = 0; i < 16; i++) {
            System.out.println(toBin(i));
        }
    }
}
