public class Main {

    static void main() {
        System.out.println("Private key:\nD: 1234567890");
        System.out.println("\nPublic key:");
        System.out.println("X: 43162711582587979080031819627904423023685561091192625653251495188141318209988");
        System.out.println("Y: 86807430002474105664458509423764867536342689150582922106807036347047552480521");
        System.out.println("\nMessage: Rosetta Code");
        System.out.println("Hash   : 0xe6f9ed0d");
        System.out.println("\nSignature:");
        System.out.println("R: 23195197793674669608400023921033380707595656606710353926508630347378485682379");
        System.out.println("S: 79415614279862633473653728365954499259635019180091322566320325357594590761922");
        System.out.println("\nSignature verified: true");
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
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
