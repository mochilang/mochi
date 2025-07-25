//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

type Client struct {
	Base        string `json:"Base"`
	Host        string `json:"Host"`
	Port        int    `json:"Port"`
	GroupFilter string `json:"GroupFilter"`
}

// line 3
func search_user(directory map[string][]string, username string) []string {
	return directory[username]
}

// line 7
func main() {
	client := Client{
		Base:        "dc=example,dc=com",
		Host:        "ldap.example.com",
		Port:        389,
		GroupFilter: "(memberUid=%s)",
	}
	_ = client
	directory := map[string][]string{"username": []string{"admins", "users"}, "john": []string{"users"}}
	groups := search_user(directory, "username")
	if len(any(groups)) > 0 {
		out := "Groups: ["
		i := 0
		for i < len(any(groups)) {
			out = out + "\"" + groups[i] + "\""
			if i < (len(any(groups)) - 1) {
				out = out + ", "
			}
			i = (i + 1)
		}
		out = out + "]"
		fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(out)), "\n"))
	} else {
		fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("User not found")), "\n"))
	}
}

func main() {
	main()
}
