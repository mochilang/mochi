public class Main {
    static Object[] res = _netLookupHost("www.kame.net");
    static Object addrs = res[0];
    static Object err = res[1];

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            if ((err == null)) {
                System.out.println(String.valueOf(addrs));
            } else {
                System.out.println(err);
            }
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

    static Object[] _netLookupHost(String host) {
        try {
            java.net.InetAddress[] arr = java.net.InetAddress.getAllByName(host);
            String[] out = new String[arr.length];
            for (int i = 0; i < arr.length; i++) { out[i] = arr[i].getHostAddress(); }
            return new Object[]{out, null};
        } catch (Exception e) {
            return new Object[]{null, e.toString()};
        }
    }
}
