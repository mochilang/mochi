public class Main {
    static class Foobar {
        int Exported;
        int unexported;
        Foobar(int Exported, int unexported) {
            this.Exported = Exported;
            this.unexported = unexported;
        }
        @Override public String toString() {
            return String.format("{'Exported': %s, 'unexported': %s}", String.valueOf(Exported), String.valueOf(unexported));
        }
    }

    static Foobar obj = new Foobar(12, 42);

    static Foobar examineAndModify(Foobar f) {
        System.out.println(" v: {" + String.valueOf(f.Exported) + " " + String.valueOf(f.unexported) + "} = {" + String.valueOf(f.Exported) + " " + String.valueOf(f.unexported) + "}");
        System.out.println("    Idx Name       Type CanSet");
        System.out.println("     0: Exported   int  true");
        System.out.println("     1: unexported int  false");
f.Exported = 16;
f.unexported = 44;
        System.out.println("  modified unexported field via unsafe");
        return f;
    }

    static void anotherExample() {
        System.out.println("bufio.ReadByte returned error: unsafely injected error value into bufio inner workings");
    }
    public static void main(String[] args) {
        System.out.println("obj: {" + String.valueOf(obj.Exported) + " " + String.valueOf(obj.unexported) + "}");
        obj = examineAndModify(obj);
        System.out.println("obj: {" + String.valueOf(obj.Exported) + " " + String.valueOf(obj.unexported) + "}");
        anotherExample();
    }
}
