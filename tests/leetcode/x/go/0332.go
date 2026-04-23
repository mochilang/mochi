package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

func findItinerary(tickets [][2]string) []string {
	graph := map[string][]string{}
	for _, t := range tickets { graph[t[0]] = append(graph[t[0]], t[1]) }
	for k := range graph { sort.Sort(sort.Reverse(sort.StringSlice(graph[k]))) }
	route := []string{}
	var visit func(string)
	visit = func(a string) {
		for len(graph[a]) > 0 {
			n := len(graph[a]) - 1
			next := graph[a][n]
			graph[a] = graph[a][:n]
			visit(next)
		}
		route = append(route, a)
	}
	visit("JFK")
	for i, j := 0, len(route)-1; i < j; i, j = i+1, j-1 { route[i], route[j] = route[j], route[i] }
	return route
}

func fmtRoute(route []string) string {
	parts := make([]string, len(route))
	for i, s := range route { parts[i] = "\"" + s + "\"" }
	return "[" + strings.Join(parts, ",") + "]"
}

func main() {
	in := bufio.NewReader(os.Stdin); out := bufio.NewWriter(os.Stdout); defer out.Flush()
	var tcases int; if _, err := fmt.Fscan(in, &tcases); err != nil { return }
	for tc := 0; tc < tcases; tc++ {
		var m int; fmt.Fscan(in, &m); tickets := make([][2]string, m)
		for i := range tickets { fmt.Fscan(in, &tickets[i][0], &tickets[i][1]) }
		if tc > 0 { fmt.Fprintln(out); fmt.Fprintln(out) }
		fmt.Fprint(out, fmtRoute(findItinerary(tickets)))
	}
}
