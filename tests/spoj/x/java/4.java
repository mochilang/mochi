public class Main {

    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static java.math.BigInteger precedence(String op) {
        if ((op.equals("+")) || (op.equals("-"))) {
            return java.math.BigInteger.valueOf(1);
        }
        if ((op.equals("*")) || (op.equals("/"))) {
            return java.math.BigInteger.valueOf(2);
        }
        if ((op.equals("^"))) {
            return java.math.BigInteger.valueOf(3);
        }
        return java.math.BigInteger.valueOf(0);
    }

    static String popTop(String[] stack) {
        return stack[_idx((stack).length, ((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(stack.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())];
    }

    static String[] popStack(String[] stack) {
        String[] newStack = ((String[])(new String[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(stack.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
            newStack = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(newStack), java.util.stream.Stream.of(stack[_idx((stack).length, ((java.math.BigInteger)(i_1)).longValue())])).toArray(String[]::new)));
            i_1 = i_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((String[])(newStack));
    }

    static String toRPN(String expr) {
        String out = "";
        String[] stack_1 = ((String[])(new String[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(expr)))) < 0) {
            String ch_1 = expr.substring((int)(((java.math.BigInteger)(i_3)).longValue()), (int)(((java.math.BigInteger)(i_3)).longValue())+1);
            if ((ch_1.compareTo("a") >= 0) && (ch_1.compareTo("z") <= 0)) {
                out = out + ch_1;
            } else             if ((ch_1.equals("("))) {
                stack_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            } else             if ((ch_1.equals(")"))) {
                while (new java.math.BigInteger(String.valueOf(stack_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    String top_2 = String.valueOf(popTop(((String[])(stack_1))));
                    if ((top_2.equals("("))) {
                        stack_1 = ((String[])(popStack(((String[])(stack_1)))));
                        break;
                    }
                    out = out + top_2;
                    stack_1 = ((String[])(popStack(((String[])(stack_1)))));
                }
            } else {
                java.math.BigInteger prec_1 = precedence(ch_1);
                while (new java.math.BigInteger(String.valueOf(stack_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    String top_3 = String.valueOf(popTop(((String[])(stack_1))));
                    if ((top_3.equals("("))) {
                        break;
                    }
                    java.math.BigInteger topPrec_1 = precedence(top_3);
                    if (topPrec_1.compareTo(prec_1) > 0 || (topPrec_1.compareTo(prec_1) == 0 && !(ch_1.equals("^")))) {
                        out = out + top_3;
                        stack_1 = ((String[])(popStack(((String[])(stack_1)))));
                    } else {
                        break;
                    }
                }
                stack_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(ch_1)).toArray(String[]::new)));
            }
            i_3 = i_3.add(java.math.BigInteger.valueOf(1));
        }
        while (new java.math.BigInteger(String.valueOf(stack_1.length)).compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            String top_5 = String.valueOf(popTop(((String[])(stack_1))));
            out = out + top_5;
            stack_1 = ((String[])(popStack(((String[])(stack_1)))));
        }
        return out;
    }

    static void main() {
        java.math.BigInteger t = new java.math.BigInteger(String.valueOf(Integer.parseInt((_scanner.hasNextLine() ? _scanner.nextLine() : ""))));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(t) < 0) {
            String expr_1 = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            System.out.println(toRPN(expr_1));
            i_5 = i_5.add(java.math.BigInteger.valueOf(1));
        }
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
