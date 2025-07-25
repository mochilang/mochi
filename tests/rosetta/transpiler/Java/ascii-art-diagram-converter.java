public class Main {

    static void main() {
        System.out.println("Diagram after trimming whitespace and removal of blank lines:\n");
        System.out.println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+");
        System.out.println("|                      ID                       |");
        System.out.println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+");
        System.out.println("|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |");
        System.out.println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+");
        System.out.println("|                    QDCOUNT                    |");
        System.out.println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+");
        System.out.println("|                    ANCOUNT                    |");
        System.out.println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+");
        System.out.println("|                    NSCOUNT                    |");
        System.out.println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+");
        System.out.println("|                    ARCOUNT                    |");
        System.out.println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+");
        System.out.println("\nDecoded:\n");
        System.out.println("Name     Bits  Start  End");
        System.out.println("=======  ====  =====  ===");
        System.out.println("ID        16      0    15");
        System.out.println("QR         1     16    16");
        System.out.println("Opcode     4     17    20");
        System.out.println("AA         1     21    21");
        System.out.println("TC         1     22    22");
        System.out.println("RD         1     23    23");
        System.out.println("RA         1     24    24");
        System.out.println("Z          3     25    27");
        System.out.println("RCODE      4     28    31");
        System.out.println("QDCOUNT   16     32    47");
        System.out.println("ANCOUNT   16     48    63");
        System.out.println("NSCOUNT   16     64    79");
        System.out.println("ARCOUNT   16     80    95");
        System.out.println("\nTest string in hex:");
        System.out.println("78477bbf5496e12e1bf169a4");
        System.out.println("\nTest string in binary:");
        System.out.println("011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100");
        System.out.println("\nUnpacked:\n");
        System.out.println("Name     Size  Bit pattern");
        System.out.println("=======  ====  ================");
        System.out.println("ID        16   0111100001000111");
        System.out.println("QR         1   0");
        System.out.println("Opcode     4   1111");
        System.out.println("AA         1   0");
        System.out.println("TC         1   1");
        System.out.println("RD         1   1");
        System.out.println("RA         1   1");
        System.out.println("Z          3   011");
        System.out.println("RCODE      4   1111");
        System.out.println("QDCOUNT   16   0101010010010110");
        System.out.println("ANCOUNT   16   1110000100101110");
        System.out.println("NSCOUNT   16   0001101111110001");
        System.out.println("ARCOUNT   16   0110100110100100");
    }
    public static void main(String[] args) {
        main();
    }
}
