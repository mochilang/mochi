// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// apply-a-callback-to-an-array-1.mochi
import java.util.*;

public class ApplyACallbackToAnArray1 {
    public static void main(String[] args) {
        for (Integer i : Arrays.asList(1, 2, 3, 4, 5)) {
            System.out.println(String.valueOf(i * i));
        }
    }
}
