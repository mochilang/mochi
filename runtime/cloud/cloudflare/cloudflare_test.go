package cloudflare

import (
    "context"
    "io"
    "net/http"
    "net/http/httptest"
    "os"
    "testing"

    cf "github.com/cloudflare/cloudflare-go"
)

func TestWorkerBindings(t *testing.T) {
	w := NewWorker("api", "index.js")
	store := NewKV("cache")
	bucket := NewBucket("assets")
	q := NewQueue("jobs")
	db := NewDatabase("main")
	trig := Every("0 * * * *")

	w.BindKV(store)
	w.BindR2(bucket)
	w.BindQueue(q)
	w.BindD1(db)
	w.OnCron(trig)
	w.Route("/api/*")

	if len(w.Bindings) != 4 {
		t.Fatalf("expected 4 bindings, got %d", len(w.Bindings))
	}
	if len(w.Routes) != 1 || w.Routes[0] != "/api/*" {
		t.Fatalf("unexpected route: %v", w.Routes)
	}
	if len(w.Crons) != 1 || w.Crons[0].Schedule != "0 * * * *" {
		t.Fatalf("unexpected cron: %v", w.Crons)
	}
}

func TestDeployWorker(t *testing.T) {
        server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
                if r.Method != http.MethodPut {
                        t.Fatalf("expected PUT, got %s", r.Method)
                }
                if r.URL.Path != "/accounts/acc/workers/scripts/test" {
                        t.Fatalf("unexpected path %s", r.URL.Path)
                }
                io.Copy(io.Discard, r.Body)
                w.Header().Set("Content-Type", "application/json")
                w.Write([]byte(`{"success":true}`))
        }))
        defer server.Close()

        api, err := cf.NewWithAPIToken("token", cf.BaseURL(server.URL))
        if err != nil {
                t.Fatal(err)
        }
        client := &Client{api: api, accountID: "acc"}

        f, err := os.CreateTemp("", "w.js")
        if err != nil {
                t.Fatal(err)
        }
        defer os.Remove(f.Name())
        f.WriteString("console.log('hi')")
        f.Close()

        w := NewWorker("test", f.Name())
        if err := client.DeployWorker(context.Background(), w); err != nil {
                t.Fatal(err)
        }
}
