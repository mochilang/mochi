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

    static String[] validate(String[] commands, String[] words, int[] mins) {
        String[] results = new String[]{};
        if (words.length == 0) {
            return results;
        }
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
            if (!found) {
                results = java.util.stream.Stream.concat(java.util.Arrays.stream(results), java.util.stream.Stream.of("*error*")).toArray(String[]::new);
            }
            wi = wi + 1;
        }
        return results;
    }

    static void main() {
        String table = "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress Copy " + "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find " + "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput " + " Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO " + "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT " + "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT " + "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer TypeUp ";
        String[] commands = fields(table);
        int[] mins = new int[]{};
        int i = 0;
        while (i < commands.length) {
            int count = 0;
            int j = 0;
            String cmd = commands[i];
            while (j < cmd.length()) {
                String ch = cmd.substring(j, j + 1);
                if (((ch.compareTo("A") >= 0) && ch.compareTo("Z") <= 0)) {
                    count = count + 1;
                }
                j = j + 1;
            }
            mins = java.util.stream.IntStream.concat(java.util.Arrays.stream(mins), java.util.stream.IntStream.of(count)).toArray();
            i = i + 1;
        }
        String sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin";
        String[] words = fields(sentence);
        String[] results = validate(commands, words, mins);
        String out1 = "user words:  ";
        int k = 0;
        while (k < words.length) {
            out1 = out1 + padRight(words[k], results[k].length()) + " ";
            k = k + 1;
        }
        System.out.println(out1);
        System.out.println("full words:  " + join(results, " "));
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
