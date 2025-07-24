public class Main {
    static class Cat {
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

    static class Dog {
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


    static String beastKind(Object b) {
        return (new java.util.function.Supplier<String>(){public String get(){
    var _m0 = b;
    if (_m0 instanceof Dog v0) {
        var k = v0.kind;
        return k;
    }
    if (_m0 instanceof Cat v1) {
        var k = v1.kind;
        return k;
    }
    return null;
}}).get();
    }

    static String beastName(Object b) {
        return (new java.util.function.Supplier<String>(){public String get(){
    var _m1 = b;
    if (_m1 instanceof Dog v0) {
        var n = v0.name;
        return n;
    }
    if (_m1 instanceof Cat v1) {
        var n = v1.name;
        return n;
    }
    return null;
}}).get();
    }

    static String beastCry(Object b) {
        return (new java.util.function.Supplier<String>(){public String get(){
    var _m2 = b;
    if (_m2 instanceof Dog v0) {
        return "Woof";
    }
    if (_m2 instanceof Cat v1) {
        return "Meow";
    }
    return null;
}}).get();
    }

    static void bprint(Object b) {
        System.out.println(beastName(b) + ", who's a " + beastKind(b) + ", cries: \"" + beastCry(b) + "\".");
    }

    static void main() {
        Object d = new Dog("labrador", "Max");
        Object c = new Cat("siamese", "Sammy");
        bprint(d);
        bprint(c);
    }
    public static void main(String[] args) {
        main();
    }
}
