package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var less20 = []string{"", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"}
var tens = []string{"", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"}
var thousands = []string{"", "Thousand", "Million", "Billion"}

func helper(n int) string {
	if n == 0 {
		return ""
	}
	if n < 20 {
		return less20[n]
	}
	if n < 100 {
		if n%10 == 0 {
			return tens[n/10]
		}
		return tens[n/10] + " " + helper(n%10)
	}
	if n%100 == 0 {
		return less20[n/100] + " Hundred"
	}
	return less20[n/100] + " Hundred " + helper(n%100)
}

func solve(num int) string {
	if num == 0 {
		return "Zero"
	}
	parts := []string{}
	for idx := 0; num > 0; idx++ {
		chunk := num % 1000
		if chunk != 0 {
			words := helper(chunk)
			if thousands[idx] != "" {
				words += " " + thousands[idx]
			}
			parts = append([]string{words}, parts...)
		}
		num /= 1000
	}
	return strings.Join(parts, " ")
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	out := make([]string, 0, t)
	for i := 0; i < t && sc.Scan(); i++ {
		num, _ := strconv.Atoi(strings.TrimSpace(sc.Text()))
		out = append(out, solve(num))
	}
	fmt.Print(strings.Join(out, "\n"))
}
