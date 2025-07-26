public class Main {
    static class Foo {
        Foo() {
        }
        void ValueMethod(int x) {
        }
        void PointerMethod(int x) {
        }
        @Override public String toString() {
            return "Foo{}";
        }
    }

    static Foo myValue = new Foo();
    static Foo myPointer = new Foo();

    public static void main(String[] args) {
        myValue.ValueMethod(0);
        myPointer.PointerMethod(0);
        myPointer.ValueMethod(0);
        myValue.PointerMethod(0);
        myValue.ValueMethod(0);
        myPointer.PointerMethod(0);
    }
}
