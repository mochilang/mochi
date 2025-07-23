public class Main {
    static String result = "";

    public static void main(String[] args) {
        for (int i = 1; i < 101; i++) {
            int j = 1;
            while (j * j < i) {
                j = j + 1;
            }
            if (j * j == i) {
                result = result + "O";
            } else {
                result = result + "-";
            }
        }
        System.out.println(result);
    }
}
