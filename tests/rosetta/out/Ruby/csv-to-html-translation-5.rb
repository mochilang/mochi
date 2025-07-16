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
def _splitString(s, sep)
  raise 'split expects string' unless s.is_a?(String)
  s.split(sep)
end

def split(s, sep)
	out = []
	start = 0
	i = 0
	n = (sep).length
	while (i <= ((s).length - n))
		if (_sliceString(s, i, (i + n)) == sep)
			out = (out + [_sliceString(s, start, i)])
			i = (i + n)
			start = i
		else
			i = (i + 1)
		end
	end
	out = (out + [_sliceString(s, start, (s).length)])
	return out
end

def htmlEscape(s)
	out = ""
	i = 0
	while (i < (s).length)
		ch = _sliceString(s, i, (i + 1))
		if (ch == "&")
			out = (out + "&amp;")
		elsif (ch == "<")
			out = (out + "&lt;")
		elsif (ch == ">")
			out = (out + "&gt;")
		else
			out = (out + ch)
		end
		i = (i + 1)
	end
	return out
end

$c = ((((("Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n") + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") + "The multitude,Who are you?\n") + "Brians mother,I'm his mother; that's who!\n") + "The multitude,Behold his mother! Behold his mother!")
$rows = []
_splitString($c, "\n").each do |line|
	$rows = ($rows + [_splitString(line, ",")])
end
puts("<table>")
$rows.each do |row|
	cells = ""
	row.each do |cell|
		cells = (((cells + "<td>") + htmlEscape(cell)) + "</td>")
	end
	puts((("    <tr>" + cells) + "</tr>"))
end
puts("</table>")
