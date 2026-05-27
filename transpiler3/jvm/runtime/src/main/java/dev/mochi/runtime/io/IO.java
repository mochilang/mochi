package dev.mochi.runtime.io;

/** Mochi println dispatch for all scalar types. */
public final class IO {
    private IO() {}
    public static void println(long v) { System.out.println(v); }
    public static void println(double v) { System.out.println(v); }
    public static void println(boolean v) { System.out.println(v); }
    public static void println(String v) { System.out.println(v); }
    public static void println(Object v) { System.out.println(v); }
}
