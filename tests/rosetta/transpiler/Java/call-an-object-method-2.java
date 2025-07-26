public class Main {
    static fn():void[] funcs = newFactory();
    static fn():void New = funcs[0];
    static fn():void Count = funcs[1];
    static class Box {
        String Contents;
        int secret;
        Box(String Contents, int secret) {
            this.Contents = Contents;
            this.secret = secret;
        }
        int TellSecret() {
            return secret;
        }
        @Override public String toString() {
            return String.format("{'Contents': '%s', 'secret': %s}", String.valueOf(Contents), String.valueOf(secret));
        }
    }


    static fn():void[] newFactory() {
        int sn = 0;
        fn():Box New = () -> {
    sn = sn + 1;
    static Box b = new Box(sn);
    if (sn == 1) {
b.Contents = "rabbit";
    } else     if (sn == 2) {
b.Contents = "rock";
    }
    return b;
};
        fn():int Count = () -> sn;
        return new fn():void[]{New, Count};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
