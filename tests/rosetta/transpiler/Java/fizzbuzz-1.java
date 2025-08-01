public class Main {

    public static void main(String[] args) {
        for (int i = 1; i < 101; i++) {
            if (Math.floorMod(i, 15) == 0) {
                System.out.println("FizzBuzz");
            } else             if (Math.floorMod(i, 3) == 0) {
                System.out.println("Fizz");
            } else             if (Math.floorMod(i, 5) == 0) {
                System.out.println("Buzz");
            } else {
                System.out.println(String.valueOf(i));
            }
        }
    }
}
