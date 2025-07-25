public class Main {

    static String[] writeTwo() {
        return new String[]{"jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash", "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash"};
    }

    static String[] appendOneMore(String[] lines) {
        return java.util.stream.Stream.concat(java.util.Arrays.stream(lines), java.util.stream.Stream.of("xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash")).toArray(String[]::new);
    }

    static void main() {
        String[] lines = writeTwo();
        lines = appendOneMore(lines);
        if ((lines.length >= 3 && lines[2].equals("xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"))) {
            System.out.println("append okay");
        } else {
            System.out.println("it didn't work");
        }
    }
    public static void main(String[] args) {
        main();
    }
}
