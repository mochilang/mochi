public class Main {
    static class SomeStruct {
        java.util.Map<String,String> runtimeFields;
        SomeStruct(java.util.Map<String,String> runtimeFields) {
            this.runtimeFields = runtimeFields;
        }
        @Override public String toString() {
            return String.format("{'runtimeFields': %s}", String.valueOf(runtimeFields));
        }
    }


    static java.util.Scanner _scanner = new java.util.Scanner(System.in);

    static void main() {
        SomeStruct ss = new SomeStruct(new java.util.LinkedHashMap<String, String>());
        System.out.println("Create two fields at runtime: \n");
        int i = 1;
        while (i <= 2) {
            System.out.println(String.valueOf("  Field #" + String.valueOf(i)) + ":\n");
            System.out.println("       Enter name  : ");
            String name = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            System.out.println("       Enter value : ");
            String value = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            java.util.Map<String,String> fields = ss.runtimeFields;
fields.put(name, value);
ss.runtimeFields = fields;
            System.out.println("\n");
            i = i + 1;
        }
        while (true) {
            System.out.println("Which field do you want to inspect ? ");
            String name = (_scanner.hasNextLine() ? _scanner.nextLine() : "");
            if (ss.runtimeFields.containsKey(name)) {
                Object value = (Object)(((Object)((java.util.Map)ss.runtimeFields).get(name)));
                System.out.println(String.valueOf("Its value is '" + value) + "'");
                return;
            } else {
                System.out.println("There is no field of that name, try again\n");
            }
        }
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
