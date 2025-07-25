//go:build ignore

// Generated by Mochi v0.10.39 on 2025-07-25 09:23:19 GMT+7
package main

import (
	"fmt"
)

type LDAPClient struct {
	Base         string   `json:"Base"`
	Host         string   `json:"Host"`
	Port         int      `json:"Port"`
	UseSSL       bool     `json:"UseSSL"`
	BindDN       string   `json:"BindDN"`
	BindPassword string   `json:"BindPassword"`
	UserFilter   string   `json:"UserFilter"`
	GroupFilter  string   `json:"GroupFilter"`
	Attributes   []string `json:"Attributes"`
}

func connect(client LDAPClient) bool {
	return ((client.Host != "") && (client.Port > 0))
}

func mochiMain() {
	var client LDAPClient = LDAPClient{
		Base:         "dc=example,dc=com",
		Host:         "ldap.example.com",
		Port:         389,
		UseSSL:       false,
		BindDN:       "uid=readonlyuser,ou=People,dc=example,dc=com",
		BindPassword: "readonlypassword",
		UserFilter:   "(uid=%s)",
		GroupFilter:  "(memberUid=%s)",
		Attributes:   []string{"givenName", "sn", "mail", "uid"},
	}
	_ = client
	if connect(client) {
		fmt.Println(("Connected to " + client.Host))
	} else {
		fmt.Println("Failed to connect")
	}
}

func main() {
	mochiMain()
}
