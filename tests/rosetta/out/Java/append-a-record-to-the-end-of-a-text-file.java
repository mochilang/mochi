// append-a-record-to-the-end-of-a-text-file.mochi
import java.util.*;

public class AppendARecordToTheEndOfATextFile {
    static List<String> writeTwo() {
        return Arrays.asList("jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash", "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash");
    }
    static List<String> appendOneMore(List<String> lines) {
        return append(lines, "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash");
    }
    static void main() {
        List<String> lines = writeTwo();
        lines = appendOneMore(lines);
        if (lines.size() >= 3 && Objects.equals(lines.get(2), "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash")) {
            System.out.println("append okay");
        }
        else {
            System.out.println("it didn't work");
        }
    }
    static <T> List<T> append(List<T> list, T item) {
        List<T> res = new ArrayList<>(list);
        res.add(item);
        return res;
    }
    public static void main(String[] args) {
    main();
    }
}
