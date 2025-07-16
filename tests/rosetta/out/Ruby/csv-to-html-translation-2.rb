def _splitString(s, sep)
  raise 'split expects string' unless s.is_a?(String)
  s.split(sep)
end

$c = ((((("Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n") + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") + "The multitude,Who are you?\n") + "Brians mother,I'm his mother; that's who!\n") + "The multitude,Behold his mother! Behold his mother!")
$rows = []
_splitString($c, "\n").each do |line|
	$rows = ($rows + [_splitString(line, ",")])
end
$headings = true
puts("<table>")
if $headings
	if (($rows).length > 0)
		th = ""
		$rows[0].each do |h|
			th = (((th + "<th>") + h) + "</th>")
		end
		puts("   <thead>")
		puts((("      <tr>" + th) + "</tr>"))
		puts("   </thead>")
		puts("   <tbody>")
		i = 1
		while (i < ($rows).length)
			cells = ""
			$rows[i].each do |cell|
				cells = (((cells + "<td>") + cell) + "</td>")
			end
			puts((("      <tr>" + cells) + "</tr>"))
			i = (i + 1)
		end
		puts("   </tbody>")
	end
else
	$rows.each do |row|
		cells = ""
		row.each do |cell|
			cells = (((cells + "<td>") + cell) + "</td>")
		end
		puts((("    <tr>" + cells) + "</tr>"))
	end
end
puts("</table>")
