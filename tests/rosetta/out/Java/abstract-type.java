// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// abstract-type.mochi
import java.util.function.*;

class Beast {
    static class Dog extends Beast {
        String kind;
        String name;
        Dog(String kind, String name) {
            this.kind = kind;
            this.name = name;
        }
    }
    static class Cat extends Beast {
        String kind;
        String name;
        Cat(String kind, String name) {
            this.kind = kind;
            this.name = name;
        }
    }
}
public class AbstractType {
    static String beastKind(Beast b) {
        return (new java.util.function.Supplier<Object>(){public Object get(){
    var t0 = b;
    if (t0 instanceof Beast.Dog v1) {
        var k = v1.kind;
        return k;
    }
    if (t0 instanceof Beast.Cat v2) {
        var k = v2.kind;
        return k;
    }
    return null;
}}).get();
    }
    static String beastName(Beast b) {
        return (new java.util.function.Supplier<Object>(){public Object get(){
    var t3 = b;
    if (t3 instanceof Beast.Dog v4) {
        var n = v4.name;
        return n;
    }
    if (t3 instanceof Beast.Cat v5) {
        var n = v5.name;
        return n;
    }
    return null;
}}).get();
    }
    static String beastCry(Beast b) {
        return (new java.util.function.Supplier<String>(){public String get(){
    var t6 = b;
    if (t6 instanceof Beast.Dog v7) {
        return "Woof";
    }
    if (t6 instanceof Beast.Cat v8) {
        return "Meow";
    }
    return null;
}}).get();
    }
    static void bprint(Beast b) {
        System.out.println(beastName(b) + ", who's a " + beastKind(b) + ", cries: \"" + beastCry(b) + "\".");
    }
    static void main() {
        Beast d = new Beast.Dog("labrador", "Max");
        Beast c = new Beast.Cat("siamese", "Sammy");
        bprint(d);
        bprint(c);
    }
    public static void main(String[] args) {
        main();
    }
}
