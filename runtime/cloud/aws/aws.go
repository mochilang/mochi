package aws

import (
	"encoding/json"
	"net/http"
	"sync"
)

// Simple in-memory mock of AWS services exposed over HTTP
// Provides basic S3, Lambda, and DynamoDB primitives used by
// the Mochi standard library.

type Server struct {
	mu      sync.RWMutex
	buckets map[string]map[string]string
	tables  map[string]map[string]map[string]any
	lambdas map[string]func(any) any
}

func NewServer() *Server {
	return &Server{
		buckets: map[string]map[string]string{},
		tables:  map[string]map[string]map[string]any{},
		lambdas: map[string]func(any) any{},
	}
}

func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	switch {
	case r.Method == http.MethodGet && r.URL.Path == "/s3/keys":
		s.handleS3Keys(w, r)
	case r.Method == http.MethodPut && r.URL.Path == "/s3/object":
		s.handleS3Put(w, r)
	case r.Method == http.MethodGet && r.URL.Path == "/s3/object":
		s.handleS3Get(w, r)
	case r.Method == http.MethodPost && r.URL.Path == "/lambda/invoke":
		s.handleLambdaInvoke(w, r)
	case r.Method == http.MethodPut && r.URL.Path == "/dynamo/item":
		s.handleDynamoPut(w, r)
	case r.Method == http.MethodGet && r.URL.Path == "/dynamo/item":
		s.handleDynamoGet(w, r)
	case r.Method == http.MethodGet && r.URL.Path == "/dynamo/keys":
		s.handleDynamoKeys(w, r)
	default:
		http.NotFound(w, r)
	}
}

func (s *Server) handleS3Put(w http.ResponseWriter, r *http.Request) {
	bucket := r.URL.Query().Get("bucket")
	key := r.URL.Query().Get("key")
	if bucket == "" || key == "" {
		http.Error(w, "missing bucket or key", http.StatusBadRequest)
		return
	}
	var body struct {
		Value string `json:"value"`
	}
	if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	s.mu.Lock()
	b := s.buckets[bucket]
	if b == nil {
		b = map[string]string{}
		s.buckets[bucket] = b
	}
	b[key] = body.Value
	s.mu.Unlock()
	w.WriteHeader(http.StatusNoContent)
}

func (s *Server) handleS3Get(w http.ResponseWriter, r *http.Request) {
	bucket := r.URL.Query().Get("bucket")
	key := r.URL.Query().Get("key")
	if bucket == "" || key == "" {
		http.Error(w, "missing bucket or key", http.StatusBadRequest)
		return
	}
	s.mu.RLock()
	val := ""
	if b := s.buckets[bucket]; b != nil {
		val = b[key]
	}
	s.mu.RUnlock()
	json.NewEncoder(w).Encode(map[string]string{"value": val})
}

func (s *Server) handleS3Keys(w http.ResponseWriter, r *http.Request) {
	bucket := r.URL.Query().Get("bucket")
	if bucket == "" {
		http.Error(w, "missing bucket", http.StatusBadRequest)
		return
	}
	s.mu.RLock()
	keys := []string{}
	if b := s.buckets[bucket]; b != nil {
		for k := range b {
			keys = append(keys, k)
		}
	}
	s.mu.RUnlock()
	json.NewEncoder(w).Encode(map[string]any{"keys": keys})
}

func (s *Server) handleDynamoPut(w http.ResponseWriter, r *http.Request) {
	table := r.URL.Query().Get("table")
	key := r.URL.Query().Get("key")
	if table == "" || key == "" {
		http.Error(w, "missing table or key", http.StatusBadRequest)
		return
	}
	var item map[string]any
	if err := json.NewDecoder(r.Body).Decode(&item); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	s.mu.Lock()
	t := s.tables[table]
	if t == nil {
		t = map[string]map[string]any{}
		s.tables[table] = t
	}
	t[key] = item
	s.mu.Unlock()
	w.WriteHeader(http.StatusNoContent)
}

func (s *Server) handleDynamoGet(w http.ResponseWriter, r *http.Request) {
	table := r.URL.Query().Get("table")
	key := r.URL.Query().Get("key")
	if table == "" || key == "" {
		http.Error(w, "missing table or key", http.StatusBadRequest)
		return
	}
	s.mu.RLock()
	item := map[string]any{}
	if t := s.tables[table]; t != nil {
		if it, ok := t[key]; ok {
			item = it
		}
	}
	s.mu.RUnlock()
	json.NewEncoder(w).Encode(item)
}

func (s *Server) handleDynamoKeys(w http.ResponseWriter, r *http.Request) {
	table := r.URL.Query().Get("table")
	if table == "" {
		http.Error(w, "missing table", http.StatusBadRequest)
		return
	}
	s.mu.RLock()
	keys := []string{}
	if t := s.tables[table]; t != nil {
		for k := range t {
			keys = append(keys, k)
		}
	}
	s.mu.RUnlock()
	json.NewEncoder(w).Encode(map[string]any{"keys": keys})
}

func (s *Server) handleLambdaInvoke(w http.ResponseWriter, r *http.Request) {
	name := r.URL.Query().Get("name")
	if name == "" {
		http.Error(w, "missing name", http.StatusBadRequest)
		return
	}
	var payload any
	if err := json.NewDecoder(r.Body).Decode(&payload); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	s.mu.RLock()
	fn := s.lambdas[name]
	s.mu.RUnlock()
	if fn == nil {
		http.Error(w, "function not found", http.StatusNotFound)
		return
	}
	result := fn(payload)
	json.NewEncoder(w).Encode(map[string]any{"result": result})
}

func (s *Server) RegisterLambda(name string, fn func(any) any) {
	s.mu.Lock()
	s.lambdas[name] = fn
	s.mu.Unlock()
}
