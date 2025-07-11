//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 3
func search_user(directory map[string][]string, username string) []string {
	return directory[username]
}

// line 7
func main() {
	type Client struct {
		Base        string `json:"Base"`
		Host        string `json:"Host"`
		Port        int    `json:"Port"`
		GroupFilter string `json:"GroupFilter"`
	}

	var client Client = Client{
		Base:        "dc=example,dc=com",
		Host:        "ldap.example.com",
		Port:        389,
		GroupFilter: "(memberUid=%s)",
	}
	_ = client
	type Directory struct {
		Username []string `json:"username"`
		John     []string `json:"john"`
	}

	var directory Directory = Directory{
		Username: []string{"admins", "users"},
		John:     []string{"users"},
	}
	var groups []string = search_user(_cast[map[string][]string](directory), "username")
	if len(groups) > 0 {
		var out string = "Groups: ["
		var i int = 0
		for {
			if !(i < len(groups)) {
				break
			}
			out = out + "\"" + groups[i] + "\""
			if i < (len(groups) - 1) {
				out = out + ", "
			}
			i = (i + 1)
		}
		out = out + "]"
		fmt.Println(out)
	} else {
		fmt.Println("User not found")
	}
}

func main() {
	main()
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
	}
	return out
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
