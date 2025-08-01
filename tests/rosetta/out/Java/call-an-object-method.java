// Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
// call-an-object-method.mochi
import java.util.*;

class Box {
    String Contents;
    int secret;
    Box(String Contents, int secret) {
        this.Contents = Contents;
        this.secret = secret;
    }
    @Override public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Box other)) return false;
        return Objects.equals(this.Contents, other.Contents) && Objects.equals(this.secret, other.secret);
    }
    @Override public int hashCode() {
        return Objects.hash(Contents, secret);
    }
}
public class CallAnObjectMethod {
    static Box New() {
        Box b = new Box("rabbit", 1);
        return b;
    }
    public static void main(String[] args) {
        Box box = New();
        box.TellSecret();
    }
}
