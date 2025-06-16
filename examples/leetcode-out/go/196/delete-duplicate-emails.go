package main

import (
	"fmt"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type User struct {
	Id int `json:"id"`
	Email string `json:"email"`
}

func deleteDuplicateEmails(users []User) []User {
	var byEmail map[string]User = map[string]User{}
	for _, u := range users {
		_tmp0 := u.Email
		_tmp1 := byEmail
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			var existing User = byEmail[u.Email]
			_ = existing
			if (u.Id < existing.Id) {
				byEmail[u.Email] = u
			}
		} else {
			byEmail[u.Email] = u
		}
	}
	var result []User = []User{}
	for key := range byEmail {
		result = append(append([]User{}, result...), []User{byEmail[key]}...)
	}
	var sorted []User = func() []User {
	items := []User{}
	for _, x := range result {
		items = append(items, x)
	}
	type pair struct { item User; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		x := it
		pairs[idx] = pair{item: it, key: x.Id}
	}
	sort.Slice(pairs, func(i, j int) bool {
		a, b := pairs[i].key, pairs[j].key
		switch av := a.(type) {
		case int:
			switch bv := b.(type) {
			case int:
				return av < bv
			case float64:
				return float64(av) < bv
			}
		case float64:
			switch bv := b.(type) {
			case int:
				return av < float64(bv)
			case float64:
				return av < bv
			}
		case string:
			bs, _ := b.(string)
			return av < bs
		}
		return fmt.Sprint(a) < fmt.Sprint(b)
	})
	for idx, p := range pairs {
		items[idx] = p.item
	}
	_res := []User{}
	for _, x := range items {
		_res = append(_res, x)
	}
	return _res
}()
	return sorted
}

func remove_duplicates() {
	expect((fmt.Sprint(deleteDuplicateEmails(users)) == fmt.Sprint(expected)))
}

var users []User = []User{User{Id: 1, Email: "a@b.com"}, User{Id: 2, Email: "c@d.com"}, User{Id: 3, Email: "a@b.com"}}
var expected []User = []User{User{Id: 1, Email: "a@b.com"}, User{Id: 2, Email: "c@d.com"}}
func main() {
	remove_duplicates()
}

