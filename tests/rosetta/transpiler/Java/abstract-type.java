public class Main {
    interface Beast {}

    static class Dog implements Beast {
        String kind;
        String name;
        Dog(String kind, String name) {
            this.kind = kind;
            this.name = name;
        }
        @Override public String toString() {
            return String.format("{'kind': '%s', 'name': '%s'}", String.valueOf(kind), String.valueOf(name));
        }
    }

    static class Cat implements Beast {
        String kind;
        String name;
        Cat(String kind, String name) {
            this.kind = kind;
            this.name = name;
        }
        @Override public String toString() {
            return String.format("{'kind': '%s', 'name': '%s'}", String.valueOf(kind), String.valueOf(name));
        }
    }


    static String beastKind(Beast b) {
        return b instanceof Dog ? ((Dog)(b)).kind : ((Cat)(b)).kind;
    }

    static String beastName(Beast b) {
        return b instanceof Dog ? ((Dog)(b)).name : ((Cat)(b)).name;
    }

    static String beastCry(Beast b) {
        return b instanceof Dog ? "Woof" : "Meow";
    }

    static void bprint(Beast b) {
        System.out.println(beastName(b) + ", who's a " + beastKind(b) + ", cries: \"" + beastCry(b) + "\".");
    }

    static void main() {
        Beast d = new Dog("labrador", "Max");
        Beast c = new Cat("siamese", "Sammy");
        bprint(d);
        bprint(c);
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
