//go:build ignore

package main

import (
	"fmt"
)

// line 1
func main() {
	fmt.Println("Diagram after trimming whitespace and removal of blank lines:\n")
	fmt.Println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
	fmt.Println("|                      ID                       |")
	fmt.Println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
	fmt.Println("|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |")
	fmt.Println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
	fmt.Println("|                    QDCOUNT                    |")
	fmt.Println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
	fmt.Println("|                    ANCOUNT                    |")
	fmt.Println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
	fmt.Println("|                    NSCOUNT                    |")
	fmt.Println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
	fmt.Println("|                    ARCOUNT                    |")
	fmt.Println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
	fmt.Println("\nDecoded:\n")
	fmt.Println("Name     Bits  Start  End")
	fmt.Println("=======  ====  =====  ===")
	fmt.Println("ID        16      0    15")
	fmt.Println("QR         1     16    16")
	fmt.Println("Opcode     4     17    20")
	fmt.Println("AA         1     21    21")
	fmt.Println("TC         1     22    22")
	fmt.Println("RD         1     23    23")
	fmt.Println("RA         1     24    24")
	fmt.Println("Z          3     25    27")
	fmt.Println("RCODE      4     28    31")
	fmt.Println("QDCOUNT   16     32    47")
	fmt.Println("ANCOUNT   16     48    63")
	fmt.Println("NSCOUNT   16     64    79")
	fmt.Println("ARCOUNT   16     80    95")
	fmt.Println("\nTest string in hex:")
	fmt.Println("78477bbf5496e12e1bf169a4")
	fmt.Println("\nTest string in binary:")
	fmt.Println("011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100")
	fmt.Println("\nUnpacked:\n")
	fmt.Println("Name     Size  Bit pattern")
	fmt.Println("=======  ====  ================")
	fmt.Println("ID        16   0111100001000111")
	fmt.Println("QR         1   0")
	fmt.Println("Opcode     4   1111")
	fmt.Println("AA         1   0")
	fmt.Println("TC         1   1")
	fmt.Println("RD         1   1")
	fmt.Println("RA         1   1")
	fmt.Println("Z          3   011")
	fmt.Println("RCODE      4   1111")
	fmt.Println("QDCOUNT   16   0101010010010110")
	fmt.Println("ANCOUNT   16   1110000100101110")
	fmt.Println("NSCOUNT   16   0001101111110001")
	fmt.Println("ARCOUNT   16   0110100110100100")
}

func main() {
	main()
}
