public class Main {
    static int nextPID;

    static void run(boolean hasArg) {
        int pid = nextPID;
        nextPID = nextPID + 1;
        System.out.println("PID: " + _p(pid));
        if (!(Boolean)hasArg) {
            System.out.println("Done.");
            return;
        }
        int childPID = nextPID;
        System.out.println("Child's PID: " + _p(childPID));
        run(false);
    }
    public static void main(String[] args) {
        nextPID = 1;
        run(true);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
