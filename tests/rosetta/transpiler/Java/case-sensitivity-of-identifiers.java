public class Main {

    static void main() {
        String pkg_dog = "Salt";
        String Dog = "Pepper";
        String pkg_DOG = "Mustard";
        Fn3<String,String,String,java.util.Map<String,Boolean>> packageSees = (d1, d2, d3) -> {
        System.out.println("Package sees: " + d1 + " " + d2 + " " + d3);
        return new java.util.LinkedHashMap<String, Boolean>(java.util.Map.ofEntries(java.util.Map.entry("pkg_dog", true), java.util.Map.entry("Dog", true), java.util.Map.entry("pkg_DOG", true)));
};
        java.util.Map<String,Boolean> d = packageSees.apply(pkg_dog, Dog, pkg_DOG);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.\n");
        String dog = "Benjamin";
        d = packageSees.apply(pkg_dog, Dog, pkg_DOG);
        System.out.println("Main sees:   " + dog + " " + Dog + " " + pkg_DOG);
d.put("dog", true);
d.put("Dog", true);
d.put("pkg_DOG", true);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.\n");
        Dog = "Samba";
        d = packageSees.apply(pkg_dog, Dog, pkg_DOG);
        System.out.println("Main sees:   " + dog + " " + Dog + " " + pkg_DOG);
d.put("dog", true);
d.put("Dog", true);
d.put("pkg_DOG", true);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.\n");
        String DOG = "Bernie";
        d = packageSees.apply(pkg_dog, Dog, pkg_DOG);
        System.out.println("Main sees:   " + dog + " " + Dog + " " + DOG);
d.put("dog", true);
d.put("Dog", true);
d.put("pkg_DOG", true);
d.put("DOG", true);
        System.out.println("There are " + String.valueOf(d.size()) + " dogs.");
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

    @FunctionalInterface interface Fn3<A,B,C,R> { R apply(A a, B b, C c); }
    @FunctionalInterface interface Fn3Void<A,B,C> { void apply(A a, B b, C c); }
}
