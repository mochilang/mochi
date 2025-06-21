package cloud

import (
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"path/filepath"
	"strings"
	"testing"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/mod"
	"mochi/types"
)

func TestServer_BucketQueueHTTP(t *testing.T) {
	srv := NewServer()
	ts := httptest.NewServer(srv.Handler())
	defer ts.Close()

	resp, err := http.Post(ts.URL+"/v1/buckets/test/object?key=a", "application/json", strings.NewReader(`{"x":1}`))
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()

	res, err := http.Get(ts.URL + "/v1/buckets/test/object?key=a")
	if err != nil {
		t.Fatal(err)
	}
	data, _ := io.ReadAll(res.Body)
	res.Body.Close()
	var obj map[string]any
	if err := json.Unmarshal(data, &obj); err != nil {
		t.Fatalf("bad json: %v", err)
	}
	if obj["x"].(float64) != 1 {
		t.Fatalf("unexpected object: %v", obj)
	}

	_, err = http.Post(ts.URL+"/v1/queues/q/push", "application/json", strings.NewReader(`"hello"`))
	if err != nil {
		t.Fatal(err)
	}
	res, err = http.Get(ts.URL + "/v1/queues/q/pop")
	if err != nil {
		t.Fatal(err)
	}
	data, _ = io.ReadAll(res.Body)
	res.Body.Close()
	var msg string
	if err := json.Unmarshal(data, &msg); err != nil {
		t.Fatalf("bad json: %v", err)
	}
	if msg != "hello" {
		t.Fatalf("unexpected msg: %s", msg)
	}
}

func TestServer_MochiIntegration(t *testing.T) {
	srv := NewServer()
	ts := httptest.NewServer(srv.Handler())
	defer ts.Close()

	src := `import "lib/cloud" as cloud
let b = cloud.bucket("files", "` + ts.URL + `", {})
cloud.bucket_put(b, "foo", {v:1})
json(cloud.bucket_get(b, "foo"))
let q = cloud.queue("q", "` + ts.URL + `", {})
cloud.queue_push(q, {msg:"hi"})
json(cloud.queue_pop(q))`

	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	abs, _ := filepath.Abs("..")
	modRoot, _ := mod.FindRoot(abs)
	out := &strings.Builder{}
	interp := interpreter.New(prog, env, modRoot)
	interp.Env().SetWriter(out)
	if err := interp.Run(); err != nil {
		t.Fatal(err)
	}
	s := out.String()
	if !strings.Contains(s, "\"v\": 1") || !strings.Contains(s, "\"msg\": \"hi\"") {
		t.Fatalf("unexpected output:\n%s", s)
	}
}

func TestServerInterfaces(t *testing.T) {
	srv := NewServer()
	b := srv.Bucket("x")
	b.Put("k", 1)
	if v, ok := b.Get("k"); !ok || v.(int) != 1 {
		t.Fatalf("unexpected bucket get: %v %v", v, ok)
	}
	if keys := b.List("k"); len(keys) != 1 || keys[0] != "k" {
		t.Fatalf("unexpected list: %v", keys)
	}
	q := srv.Queue("q")
	q.Push("m")
	if v, ok := q.Pop(); !ok || v.(string) != "m" {
		t.Fatalf("unexpected pop: %v %v", v, ok)
	}
}
