import java.util.*;
import java.util.function.*;

public class MatchFull {
    static String classify(int n) {
        return (new java.util.function.Supplier<String>(){public String get(){
    var _t3 = n;
    if (Objects.equals(_t3, 0)) return "zero";
    else if (Objects.equals(_t3, 1)) return "one";
    return "many";
}}).get();
    }
    public static void main(String[] args) {
    int x = 2;
    Object label = (new java.util.function.Supplier<String>(){public String get(){
    var _t0 = x;
    if (Objects.equals(_t0, 1)) return "one";
    else if (Objects.equals(_t0, 2)) return "two";
    else if (Objects.equals(_t0, 3)) return "three";
    return "unknown";
}}).get();
    System.out.println(label);
    String day = "sun";
    Object mood = (new java.util.function.Supplier<String>(){public String get(){
    var _t1 = day;
    if (Objects.equals(_t1, "mon")) return "tired";
    else if (Objects.equals(_t1, "fri")) return "excited";
    else if (Objects.equals(_t1, "sun")) return "relaxed";
    return "normal";
}}).get();
    System.out.println(mood);
    boolean ok = true;
    Object status = (new java.util.function.Supplier<String>(){public String get(){
    var _t2 = ok;
    if (Objects.equals(_t2, true)) return "confirmed";
    else if (Objects.equals(_t2, false)) return "denied";
    return null;
}}).get();
    System.out.println(status);
    System.out.println(classify(0));
    System.out.println(classify(5));
    }
}
