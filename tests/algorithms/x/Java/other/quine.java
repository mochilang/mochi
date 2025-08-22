public class Main {
    static String code = "/*\nQuine:\n\nA quine is a program that outputs its own source code using string substitution.\n*/\nlet code: string = %s\nprint(code)\n";

    public static void main(String[] args) {
        System.out.println(code);
    }
}
