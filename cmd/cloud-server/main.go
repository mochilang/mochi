package main

import (
	"flag"
	"log"
	"net/http"

	"mochi/runtime/cloud"
)

func main() {
	addr := flag.String("addr", ":7777", "listen address")
	flag.Parse()
	srv := cloud.NewServer()
	log.Printf("listening on %s", *addr)
	if err := http.ListenAndServe(*addr, srv.Handler()); err != nil {
		log.Fatal(err)
	}
}
