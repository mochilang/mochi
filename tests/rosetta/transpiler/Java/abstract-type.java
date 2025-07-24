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
        main();
    }
}
