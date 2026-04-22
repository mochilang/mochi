import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayDeque;

public class Main {
    private static int calculate(String expr) {
        int result = 0;
        int number = 0;
        int sign = 1;
        ArrayDeque<Integer> stack = new ArrayDeque<>();
        for (int i = 0; i < expr.length(); i++) {
            char ch = expr.charAt(i);
            if (Character.isDigit(ch)) {
                number = number * 10 + (ch - '0');
            } else if (ch == '+' || ch == '-') {
                result += sign * number;
                number = 0;
                sign = ch == '+' ? 1 : -1;
            } else if (ch == '(') {
                stack.push(result);
                stack.push(sign);
                result = 0;
                number = 0;
                sign = 1;
            } else if (ch == ')') {
                result += sign * number;
                number = 0;
                int prevSign = stack.pop();
                int prevResult = stack.pop();
                result = prevResult + prevSign * result;
            }
        }
        return result + sign * number;
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        String line = reader.readLine();
        if (line == null) {
            return;
        }
        int t = Integer.parseInt(line.trim());
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < t; i++) {
            String expr = reader.readLine();
            if (i > 0) {
                out.append('\n');
            }
            out.append(calculate(expr));
        }
        System.out.print(out);
    }
}
