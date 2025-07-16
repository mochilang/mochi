def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end

def padLeft(s, w)
	res = ""
	n = (w - (s).length)
	while (n > 0)
		res = (res + " ")
		n = (n - 1)
	end
	return (res + s)
end

$dna = (((((((((("" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT")
puts("SEQUENCE:")
$le = ($dna).length
$i = 0
while ($i < $le)
	k = ($i + 50)
	if (k > $le)
		k = $le
	end
	puts(((padLeft(($i).to_s, 5) + ": ") + _sliceString($dna, $i, k)))
	$i = ($i + 50)
end
$a = 0
$c = 0
$g = 0
$t = 0
$idx = 0
while ($idx < $le)
	ch = _sliceString($dna, $idx, ($idx + 1))
	if (ch == "A")
		$a = ($a + 1)
	else
		if (ch == "C")
			$c = ($c + 1)
		else
			if (ch == "G")
				$g = ($g + 1)
			else
				if (ch == "T")
					$t = ($t + 1)
				end
			end
		end
	end
	$idx = ($idx + 1)
end
puts("")
puts("BASE COUNT:")
puts(("    A: " + padLeft(($a).to_s, 3)))
puts(("    C: " + padLeft(($c).to_s, 3)))
puts(("    G: " + padLeft(($g).to_s, 3)))
puts(("    T: " + padLeft(($t).to_s, 3)))
puts("    ------")
puts(("    Σ: " + ($le).to_s))
puts("    ======")
