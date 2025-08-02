public class Main {
    static String source;

    static double log2(double x) {
        double k = 0.0;
        double v = x;
        while (v >= 2.0) {
            v = v / 2.0;
            k = k + 1.0;
        }
        while (v < 1.0) {
            v = v * 2.0;
            k = k - 1.0;
        }
        double z = (v - 1.0) / (v + 1.0);
        double zpow = z;
        double sum = z;
        int i = 3;
        while (i <= 9) {
            zpow = zpow * z * z;
            sum = sum + zpow / (((Number)(i)).doubleValue());
            i = i + 2;
        }
        double ln2 = 0.6931471805599453;
        return k + 2.0 * sum / ln2;
    }

    static double entropy(String data) {
        if ((data.equals(""))) {
            return 0.0;
        }
        java.util.Map<String,Integer> counts = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        int i_1 = 0;
        while (i_1 < _runeLen(data)) {
            String ch = _substr(data, i_1, i_1 + 1);
            if (counts.containsKey(ch)) {
counts.put(ch, (int)(((int)(counts).getOrDefault(ch, 0))) + 1);
            } else {
counts.put(ch, 1);
            }
            i_1 = i_1 + 1;
        }
        double e = 0.0;
        double l = ((Number)(_runeLen(data))).doubleValue();
        for (String ch : counts.keySet()) {
            double px = (((double)(counts).getOrDefault(ch, 0))) / l;
            if (px > 0.0) {
                e = e - px * log2(px);
            }
        }
        return e;
    }

    static void main() {
        System.out.println("Source file entropy: " + String.valueOf(entropy(source)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            source = "// Mochi translation of the Rosetta \"Entropy-Narcissist\" task\n" + "// Simplified to compute the entropy of this source string\n\n" + "fun log2(x: float): float {\n" + "  var k = 0.0\n" + "  var v = x\n" + "  while v >= 2.0 {\n" + "    v = v / 2.0\n" + "    k = k + 1.0\n" + "  }\n" + "  while v < 1.0 {\n" + "    v = v * 2.0\n" + "    k = k - 1.0\n" + "  }\n" + "  let z = (v - 1.0) / (v + 1.0)\n" + "  var zpow = z\n" + "  var sum = z\n" + "  var i = 3\n" + "  while i <= 9 {\n" + "    zpow = zpow * z * z\n" + "    sum = sum + zpow / (i as float)\n" + "    i = i + 2\n" + "  }\n" + "  let ln2 = 0.6931471805599453\n" + "  return k + 2.0 * sum / ln2\n" + "}\n\n" + "fun entropy(data: string): float {\n" + "  if data == \"\" { return 0.0 }\n" + "  var counts: map<string,int> = {}\n" + "  var i = 0\n" + "  while i < len(data) {\n" + "    let ch = substring(data, i, i+1)\n" + "    if ch in counts {\n" + "      counts[ch] = counts[ch] + 1\n" + "    } else {\n" + "      counts[ch] = 1\n" + "    }\n" + "    i = i + 1\n" + "  }\n" + "  var e = 0.0\n" + "  let l = len(data) as float\n" + "  for ch in counts {\n" + "    let px = (counts[ch] as float) / l\n" + "    if px > 0.0 {\n" + "      e = e - px * log2(px)\n" + "    }\n" + "  }\n" + "  return e\n" + "}\n\n" + "// Store the program source as a string constant\n" + "let source = ... // truncated in actual source\n" + "\nfun main() {\n" + "  print(\"Source file entropy: \" + str(entropy(source)))\n" + "}\n\n" + "main()\n";
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
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
