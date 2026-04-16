//go:build slow

// spoj-index fetches and updates the SPOJ problem index for all sections.
//
// Usage:
//
//	go run -tags=slow ./cmd/spoj-index [flags]
//	  --section   classical|tutorial|challenge|partial|all  (default: all)
//	  --pages     max pages per section, 50 problems/page   (default: 100)
//	  --cookie    SPOJ session cookie string, e.g. "SPOJ_SESS=abc123; ..."
//	              Obtain from browser DevTools → Application → Cookies after login.
package main

import (
	"flag"
	"fmt"
	"log"
	"mochi/tools/spoj"
)

func main() {
	sec := flag.String("section", "all", "section: classical, tutorial, challenge, partial, or all")
	pages := flag.Int("pages", 100, "max pages per section (50 problems each)")
	cookie := flag.String("cookie", "", "SPOJ session cookie (from browser DevTools after login)")
	flag.Parse()

	if *cookie != "" {
		spoj.SessionCookie = *cookie
	}

	sections := spoj.AllSections
	if *sec != "all" {
		sections = []spoj.Section{spoj.Section(*sec)}
	}

	total := 0
	for _, s := range sections {
		fmt.Printf("Fetching section: %s\n", s)
		for page := 0; page < *pages; page++ {
			probs, err := spoj.ListSection(s, page)
			if err != nil {
				fmt.Printf("  page %d: stopped (%v)\n", page, err)
				break
			}
			fmt.Printf("  page %d: %d problems\n", page, len(probs))
			total += len(probs)
			if len(probs) < 50 {
				break
			}
		}
	}
	fmt.Printf("Total: %d problems indexed\n", total)

	if *sec == "all" {
		fmt.Println("Writing combined all.md / all.jsonl...")
		_, err := spoj.ListAllSections(*pages)
		if err != nil {
			log.Fatalf("ListAllSections: %v", err)
		}
		fmt.Println("Done.")
	}
}
