package cloud

import (
	"encoding/json"
	"net/http"
	"strings"
	"sync"
)

// Bucket defines the behavior for storing objects.
type Bucket interface {
	Get(key string) (any, bool)
	Put(key string, v any)
	Delete(key string)
	List(prefix string) []string
}

// Queue defines a simple FIFO message queue.
type Queue interface {
	Push(msg any)
	Pop() (any, bool)
}

// Server implements an in-memory bucket and queue service.
type Server struct {
	mu      sync.Mutex
	buckets map[string]map[string]any
	queues  map[string][]any
}

// Bucket returns a Bucket instance for the given name backed by the server.
func (s *Server) Bucket(name string) Bucket {
	return &memBucket{s: s, name: name}
}

// Queue returns a Queue instance for the given name backed by the server.
func (s *Server) Queue(name string) Queue {
	return &memQueue{s: s, name: name}
}

// NewServer returns a new in-memory Server.
func NewServer() *Server {
	return &Server{
		buckets: make(map[string]map[string]any),
		queues:  make(map[string][]any),
	}
}

type memBucket struct {
	s    *Server
	name string
}

func (b *memBucket) Get(key string) (any, bool) {
	b.s.mu.Lock()
	defer b.s.mu.Unlock()
	bucket, ok := b.s.buckets[b.name]
	if !ok {
		return nil, false
	}
	val, ok := bucket[key]
	return val, ok
}

func (b *memBucket) Put(key string, v any) {
	b.s.mu.Lock()
	defer b.s.mu.Unlock()
	bucket, ok := b.s.buckets[b.name]
	if !ok {
		bucket = map[string]any{}
		b.s.buckets[b.name] = bucket
	}
	bucket[key] = v
}

func (b *memBucket) Delete(key string) {
	b.s.mu.Lock()
	defer b.s.mu.Unlock()
	if bucket, ok := b.s.buckets[b.name]; ok {
		delete(bucket, key)
	}
}

func (b *memBucket) List(prefix string) []string {
	b.s.mu.Lock()
	defer b.s.mu.Unlock()
	bucket, ok := b.s.buckets[b.name]
	if !ok {
		return nil
	}
	keys := []string{}
	for k := range bucket {
		if strings.HasPrefix(k, prefix) {
			keys = append(keys, k)
		}
	}
	return keys
}

type memQueue struct {
	s    *Server
	name string
}

func (q *memQueue) Push(msg any) {
	q.s.mu.Lock()
	defer q.s.mu.Unlock()
	q.s.queues[q.name] = append(q.s.queues[q.name], msg)
}

func (q *memQueue) Pop() (any, bool) {
	q.s.mu.Lock()
	defer q.s.mu.Unlock()
	qq := q.s.queues[q.name]
	if len(qq) == 0 {
		return nil, false
	}
	msg := qq[0]
	q.s.queues[q.name] = qq[1:]
	return msg, true
}

// Handler returns an http.Handler exposing the cloud API.
func (s *Server) Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/v1/buckets/", s.handleBucket)
	mux.HandleFunc("/v1/queues/", s.handleQueue)
	return mux
}

func (s *Server) handleBucket(w http.ResponseWriter, r *http.Request) {
	parts := strings.Split(strings.TrimPrefix(r.URL.Path, "/v1/buckets/"), "/")
	if len(parts) < 2 {
		http.Error(w, "bad bucket path", http.StatusNotFound)
		return
	}
	name := parts[0]
	action := parts[1]
	b := s.Bucket(name)
	key := r.URL.Query().Get("key")
	switch action {
	case "object":
		switch r.Method {
		case http.MethodGet:
			val, ok := b.Get(key)
			if !ok {
				w.WriteHeader(http.StatusNotFound)
				return
			}
			json.NewEncoder(w).Encode(val)
		case http.MethodPost:
			var v any
			if err := json.NewDecoder(r.Body).Decode(&v); err != nil {
				http.Error(w, err.Error(), http.StatusBadRequest)
				return
			}
			b.Put(key, v)
			json.NewEncoder(w).Encode(map[string]any{"ok": true})
		case http.MethodDelete:
			b.Delete(key)
			json.NewEncoder(w).Encode(map[string]any{"ok": true})
		default:
			http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		}
	case "list":
		prefix := r.URL.Query().Get("prefix")
		json.NewEncoder(w).Encode(b.List(prefix))
	default:
		http.Error(w, "not found", http.StatusNotFound)
	}
}

func (s *Server) handleQueue(w http.ResponseWriter, r *http.Request) {
	parts := strings.Split(strings.TrimPrefix(r.URL.Path, "/v1/queues/"), "/")
	if len(parts) < 2 {
		http.Error(w, "bad queue path", http.StatusNotFound)
		return
	}
	name := parts[0]
	action := parts[1]
	q := s.Queue(name)
	switch action {
	case "push":
		if r.Method != http.MethodPost {
			http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
			return
		}
		var msg any
		if err := json.NewDecoder(r.Body).Decode(&msg); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		q.Push(msg)
		json.NewEncoder(w).Encode(map[string]any{"ok": true})
	case "pop":
		if r.Method != http.MethodGet {
			http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
			return
		}
		if msg, ok := q.Pop(); ok {
			json.NewEncoder(w).Encode(msg)
		} else {
			json.NewEncoder(w).Encode(nil)
		}
	default:
		http.Error(w, "not found", http.StatusNotFound)
	}
}
