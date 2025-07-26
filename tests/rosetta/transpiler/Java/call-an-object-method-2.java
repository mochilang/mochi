public class Main {
    static Runnable[] funcs = newFactory();
    static Runnable New = funcs[0];
    static Runnable Count = funcs[1];
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


    static Runnable[] newFactory() {
        int[] sn = new int[]{0};
        java.util.function.Supplier<Object> New = () -> {
        sn[0] = sn[0] + 1;
        Box b = new Box("", sn[0]);
        if (sn[0] == 1) {
b.Contents = "rabbit";
        } else         if (sn[0] == 2) {
b.Contents = "rock";
        }
        return b;
};
        java.util.function.Supplier<Integer> Count = () -> sn[0];
        return new Runnable[]{() -> New.get(), () -> Count.get()};
    }
    public static void main(String[] args) {
    }
}
