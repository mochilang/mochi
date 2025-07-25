public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static int randDigit() {
        return (_now() % 9) + 1;
    }

    static void main() {
        int[] digits = new int[]{};
        for (int i = 0; i < 4; i++) {
            digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(digits), java.util.stream.IntStream.of(randDigit())).toArray();
        }
        String numstr = "";
        for (int i = 0; i < 4; i++) {
            numstr = numstr + String.valueOf(digits[i]);
        }
        System.out.println("Your numbers: " + numstr + "\n");
        System.out.println("Enter RPN: ");
        String expr = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
        if (expr.length() != 7) {
            System.out.println("invalid. expression length must be 7. (4 numbers, 3 operators, no spaces)");
            return;
        }
        double[] stack = new double[]{};
        int i = 0;
        boolean valid = true;
        while (i < expr.length()) {
            String ch = expr.substring(i, i + 1);
            if (((ch.compareTo("0") >= 0) && ch.compareTo("9") <= 0)) {
                if (digits.length == 0) {
                    System.out.println("too many numbers.");
                    return;
                }
                int j = 0;
                while (digits[j] != Integer.parseInt(ch) - Integer.parseInt("0")) {
                    j = j + 1;
                    if (j == digits.length) {
                        System.out.println("wrong numbers.");
                        return;
                    }
                }
                digits = java.util.stream.IntStream.concat(java.util.Arrays.stream(java.util.Arrays.copyOfRange(digits, 0, j)), java.util.Arrays.stream(java.util.Arrays.copyOfRange(digits, j + 1, digits.length))).toArray();
                stack = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stack), java.util.stream.DoubleStream.of(((Number)(Integer.parseInt(ch) - Integer.parseInt("0"))).doubleValue())).toArray();
            } else {
                if (stack.length < 2) {
                    System.out.println("invalid expression syntax.");
                    valid = false;
                    break;
                }
                double b = stack[stack.length - 1];
                double a = stack[stack.length - 2];
                if ((ch.equals("+"))) {
stack[stack.length - 2] = a + b;
                } else                 if ((ch.equals("-"))) {
stack[stack.length - 2] = a - b;
                } else                 if ((ch.equals("*"))) {
stack[stack.length - 2] = a * b;
                } else                 if ((ch.equals("/"))) {
stack[stack.length - 2] = a / b;
                } else {
                    System.out.println(ch + " invalid.");
                    valid = false;
                    break;
                }
                stack = java.util.Arrays.copyOfRange(stack, 0, stack.length - 1);
            }
            i = i + 1;
        }
        if (valid) {
            if (((Number)(Math.abs(stack[0] - 24.0))).doubleValue() > 1e-06) {
                System.out.println("incorrect. " + String.valueOf(stack[0]) + " != 24");
            } else {
                System.out.println("correct.");
            }
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        return rt.totalMemory() - rt.freeMemory();
    }
}
