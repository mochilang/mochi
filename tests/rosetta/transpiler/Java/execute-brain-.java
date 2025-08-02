public class Main {

    static String chr(int n) {
        String upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        String lower = "abcdefghijklmnopqrstuvwxyz";
        if (n >= 65 && n < 91) {
            return upper.substring(n - 65, n - 64);
        }
        if (n >= 97 && n < 123) {
            return lower.substring(n - 97, n - 96);
        }
        if (n == 32) {
            return " ";
        }
        if (n == 33) {
            return "!";
        }
        if (n == 44) {
            return ",";
        }
        if (n == 13) {
            return "";
        }
        if (n == 10) {
            return "\n";
        }
        return "?";
    }

    static String bf(int dLen, String code) {
        int[] ds = new int[]{};
        for (int i = 0; i < dLen; i++) {
            ds = java.util.stream.IntStream.concat(java.util.Arrays.stream(ds), java.util.stream.IntStream.of(0)).toArray();
        }
        int dp = 0;
        int ip = 0;
        String out = "";
        while (ip < _runeLen(code)) {
            String ch = code.substring(ip, ip + 1);
            if ((ch.equals(">"))) {
                dp = dp + 1;
            } else             if ((ch.equals("<"))) {
                dp = dp - 1;
            } else             if ((ch.equals("+"))) {
ds[dp] = ds[dp] + 1;
            } else             if ((ch.equals("-"))) {
ds[dp] = ds[dp] - 1;
            } else             if ((ch.equals("."))) {
                out = out + String.valueOf(chr(ds[dp]));
            } else             if ((ch.equals(","))) {
            } else             if ((ch.equals("["))) {
                if (ds[dp] == 0) {
                    int nc = 1;
                    while (nc > 0) {
                        ip = ip + 1;
                        String cc = code.substring(ip, ip + 1);
                        if ((cc.equals("["))) {
                            nc = nc + 1;
                        } else                         if ((cc.equals("]"))) {
                            nc = nc - 1;
                        }
                    }
                }
            } else             if ((ch.equals("]"))) {
                if (ds[dp] != 0) {
                    int nc_1 = 1;
                    while (nc_1 > 0) {
                        ip = ip - 1;
                        String cc_1 = code.substring(ip, ip + 1);
                        if ((cc_1.equals("]"))) {
                            nc_1 = nc_1 + 1;
                        } else                         if ((cc_1.equals("["))) {
                            nc_1 = nc_1 - 1;
                        }
                    }
                }
            }
            ip = ip + 1;
        }
        return out;
    }

    static void main() {
        String prog = "++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++\n" + "++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>\n" + ">+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.\n" + "<+++++++.--------.<<<<<+.<+++.---.";
        String out_1 = String.valueOf(bf(10, prog));
        System.out.println(out_1);
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }
}
