// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// character-codes-1.mochi
import java.util.*;

public class CharacterCodes1 {
    static int ord(String ch) {
        if (Objects.equals(ch, "a")) {
            return 97;
        }
        if (Objects.equals(ch, "π")) {
            return 960;
        }
        if (Objects.equals(ch, "A")) {
            return 65;
        }
        return 0;
    }
    public static void main(String[] args) {
        System.out.println(String.valueOf(ord("a")));
        System.out.println(String.valueOf(ord("π")));
    }
}
