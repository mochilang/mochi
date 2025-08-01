# Code generated by Mochi transpiler.
# Version 0.10.39, generated on 2025-07-24 20:06 +0700
import sys
sys.set_int_max_str_digits(0)

def split(s, sep):
    out = []
    start = 0
    i = 0
    n = len(sep)
    while i <= len(s) - n:
        if s[i:i + n] == sep:
            out = out + [s[start:i]]
            i = i + n
            start = i
        else:
            i = i + 1
    out = out + [s[start:len(s)]]
    return out
def htmlEscape(s):
    out = ""
    i = 0
    while i < len(s):
        ch = s[i:i + 1]
        if ch == "&":
            out = out + "&amp;"
        else:
            if ch == "<":
                out = out + "&lt;"
            else:
                if ch == ">":
                    out = out + "&gt;"
                else:
                    out = out + ch
        i = i + 1
    return out
c = "Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n" + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n" + "The multitude,Who are you?\n" + "Brians mother,I'm his mother; that's who!\n" + "The multitude,Behold his mother! Behold his mother!"
rows = []
for line in split(c, "\n"):
    rows = rows + [split(line, ",")]
print("<table>")
for row in rows:
    cells = ""
    for cell in row:
        cells = cells + "<td>" + htmlEscape(cell) + "</td>"
    print("    <tr>" + cells + "</tr>")
print("</table>")
