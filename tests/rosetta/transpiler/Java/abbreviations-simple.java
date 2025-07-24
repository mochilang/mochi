public class Main {

    static String[] fields(String s) {
        String[] words = new String[]{};
        String cur = "";
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if ((((ch.equals(" ")) || ch.equals("\n")) || ch.equals("\t"))) {
                if (cur.length() > 0) {
                    words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
                    cur = "";
                }
            } else {
                cur = cur + ch;
            }
            i = i + 1;
        }
        if (cur.length() > 0) {
            words = java.util.stream.Stream.concat(java.util.Arrays.stream(words), java.util.stream.Stream.of(cur)).toArray(String[]::new);
        }
        return words;
    }

    static String padRight(String s, int width) {
        String out = s;
        int i = s.length();
        while (i < width) {
            out = out + " ";
            i = i + 1;
        }
        return out;
    }

    static String join(String[] xs, String sep) {
        String res = "";
        int i = 0;
        while (i < xs.length) {
            if (i > 0) {
                res = res + sep;
            }
            res = res + xs[i];
            i = i + 1;
        }
        return res;
    }

    static int parseIntStr(String str) {
        int i = 0;
        boolean neg = false;
        if ((str.length() > 0 && str.substring(0, 1).equals("-"))) {
            neg = true;
            i = 1;
        }
        int n = 0;
        java.util.Map<String,Integer> digits = new java.util.LinkedHashMap<String, Integer>(java.util.Map.of("0", 0, "1", 1, "2", 2, "3", 3, "4", 4, "5", 5, "6", 6, "7", 7, "8", 8, "9", 9));
        while (i < str.length()) {
            n = n * 10 + (int)((int)(digits.get(str.substring(i, i + 1))));
            i = i + 1;
        }
        if (neg) {
            n = -n;
        }
        return n;
    }

    static boolean isDigits(String s) {
        if (s.length() == 0) {
            return false;
        }
        int i = 0;
        while (i < s.length()) {
            String ch = s.substring(i, i + 1);
            if (((ch.compareTo("0") < 0) || ch.compareTo("9") > 0)) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static java.util.Map<String,Object> readTable(String table) {
        String[] toks = fields(table);
        String[] cmds = new String[]{};
        int[] mins = new int[]{};
        int i = 0;
        while (i < toks.length) {
            String cmd = toks[i];
            int minlen = cmd.length();
            i = i + 1;
            if (i < toks.length && isDigits(toks[i])) {
                int num = parseIntStr(toks[i]);
                if (num >= 1 && num < cmd.length()) {
                    minlen = num;
                    i = i + 1;
                }
            }
            cmds = java.util.stream.Stream.concat(java.util.Arrays.stream(cmds), java.util.stream.Stream.of(cmd)).toArray(String[]::new);
            mins = java.util.stream.IntStream.concat(java.util.Arrays.stream(mins), java.util.stream.IntStream.of(minlen)).toArray();
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.of("commands", cmds, "mins", mins));
    }

    static String[] validate(String[] commands, int[] mins, String[] words) {
        String[] results = new String[]{};
        int wi = 0;
        while (wi < words.length) {
            String w = words[wi];
            boolean found = false;
            int wlen = w.length();
            int ci = 0;
            while (ci < commands.length) {
                String cmd = commands[ci];
                if (mins[ci] != 0 && wlen >= mins[ci] && wlen <= cmd.length()) {
                    String c = cmd.toUpperCase();
                    String ww = w.toUpperCase();
                    if ((c.substring(0, wlen).equals(ww))) {
                        results = java.util.stream.Stream.concat(java.util.Arrays.stream(results), java.util.stream.Stream.of(c)).toArray(String[]::new);
                        found = true;
                        break;
                    }
                }
                ci = ci + 1;
            }
            if (!(Boolean)found) {
                results = java.util.stream.Stream.concat(java.util.Arrays.stream(results), java.util.stream.Stream.of("*error*")).toArray(String[]::new);
            }
            wi = wi + 1;
        }
        return results;
    }

    static void main() {
        String table = "" + "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 " + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate " + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 " + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load " + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 " + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 " + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left " + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 ";
        String sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin";
        java.util.Map<String,Object> tbl = readTable(table);
        String[] commands = (String[])((String[])(tbl.get("commands")));
        int[] mins = (int[])((int[])(tbl.get("mins")));
        String[] words = fields(sentence);
        String[] results = validate(commands, mins, words);
        String out1 = "user words:";
        int k = 0;
        while (k < words.length) {
            out1 = out1 + " ";
            if (k < words.length - 1) {
                out1 = out1 + padRight(words[k], results[k].length());
            } else {
                out1 = out1 + words[k];
            }
            k = k + 1;
        }
        System.out.println(out1);
        System.out.println("full words: " + join(results, " "));
    }
    public static void main(String[] args) {
        main();
    }
}
