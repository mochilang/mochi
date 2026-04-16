# SPOJ PRINTF - Printf Parser

https://www.spoj.com/problems/PRINTF/

Parse C printf statements and print their output. Format specifiers: %d (signed decimal integer), %c (character), %f (float, 6 decimal places truncated), %s (string). Read T test cases, each a single printf line.

## Approach

Find the format string between the first pair of double quotes in the printf content. Split remaining args on commas while respecting quoted strings and char literals. Walk the format string substituting each %specifier with the corresponding arg, formatting floats by truncating the fractional part to 6 digits.
