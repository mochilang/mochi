public class Main {
    static int[] data = new int[]{1, 2};
    static boolean flag = exists(new java.util.ArrayList<int>() {{ for (var x : data) { if (x == 1) { add(x); } }}});

    public static void main(String[] args) {
        System.out.println(flag);
    }
}
