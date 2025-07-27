public class Main {
    static String creature = "shark";
    static String[] pointer = new String[]{creature};

    public static void main(String[] args) {
        System.out.println("creature = " + creature);
        System.out.println("pointer = 0");
        System.out.println("*pointer = " + pointer[0]);
pointer[0] = "jellyfish";
        creature = pointer[0];
        System.out.println("*pointer = " + pointer[0]);
        System.out.println("creature = " + creature);
    }
}
