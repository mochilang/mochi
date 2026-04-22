LESS20 = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
          "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]
TENS = ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]
THOUSANDS = ["", "Thousand", "Million", "Billion"]

def helper(n)
  return "" if n == 0
  return LESS20[n] if n < 20
  return TENS[n / 10] + (n % 10 == 0 ? "" : " #{helper(n % 10)}") if n < 100
  LESS20[n / 100] + " Hundred" + (n % 100 == 0 ? "" : " #{helper(n % 100)}")
end

def solve(num)
  return "Zero" if num == 0
  parts = []
  idx = 0
  while num > 0
    chunk = num % 1000
    if chunk != 0
      words = helper(chunk)
      words += " #{THOUSANDS[idx]}" unless THOUSANDS[idx].empty?
      parts.unshift(words)
    end
    num /= 1000
    idx += 1
  end
  parts.join(" ")
end

lines = STDIN.read.lines(chomp: true)
unless lines.empty?
  t = lines[0].to_i
  puts (0...t).map { |i| solve(lines[i + 1].to_i) }
end
