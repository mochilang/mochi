package gcloud

import (
	"encoding/json"
	"io"
	"net/http"
)

// Server exposes a minimal HTTP API for Mochi fetch calls.
type Server struct{ Client Client }

// NewServer creates a new Server with the provided client.
func NewServer(c Client) *Server { return &Server{Client: c} }

// Register installs HTTP handlers on the given mux under /gcloud/.
func (s *Server) Register(mux *http.ServeMux) {
	mux.HandleFunc("/gcloud/storage/create", s.handleCreateBucket)
	mux.HandleFunc("/gcloud/storage/upload", s.handleUpload)
	mux.HandleFunc("/gcloud/storage/download", s.handleDownload)
	mux.HandleFunc("/gcloud/storage/delete", s.handleDelete)
	mux.HandleFunc("/gcloud/pubsub/createTopic", s.handleCreateTopic)
	mux.HandleFunc("/gcloud/pubsub/publish", s.handlePublish)
	mux.HandleFunc("/gcloud/pubsub/createSubscription", s.handleCreateSubscription)
	mux.HandleFunc("/gcloud/pubsub/pull", s.handlePull)
	mux.HandleFunc("/gcloud/functions/call", s.handleCallFunction)
}

func readJSON(r *http.Request, v any) error {
	data, err := io.ReadAll(r.Body)
	if err != nil {
		return err
	}
	return json.Unmarshal(data, v)
}

func writeJSON(w http.ResponseWriter, v any, err error) {
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	if v == nil {
		w.WriteHeader(http.StatusNoContent)
		return
	}
	data, e := json.Marshal(v)
	if e != nil {
		http.Error(w, e.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(data)
}

func (s *Server) handleCreateBucket(w http.ResponseWriter, r *http.Request) {
	var req struct{ Name string }
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	res, err := s.Client.CreateBucket(req.Name)
	writeJSON(w, res, err)
}
func (s *Server) handleUpload(w http.ResponseWriter, r *http.Request) {
	var req struct{ Bucket, Object, Data string }
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	writeJSON(w, nil, s.Client.Upload(req.Bucket, req.Object, req.Data))
}
func (s *Server) handleDownload(w http.ResponseWriter, r *http.Request) {
	var req struct{ Bucket, Object string }
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	res, err := s.Client.Download(req.Bucket, req.Object)
	writeJSON(w, res, err)
}
func (s *Server) handleDelete(w http.ResponseWriter, r *http.Request) {
	var req struct{ Bucket, Object string }
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	writeJSON(w, nil, s.Client.Delete(req.Bucket, req.Object))
}
func (s *Server) handleCreateTopic(w http.ResponseWriter, r *http.Request) {
	var req struct{ Name string }
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	res, err := s.Client.CreateTopic(req.Name)
	writeJSON(w, res, err)
}
func (s *Server) handlePublish(w http.ResponseWriter, r *http.Request) {
	var req struct {
		Topic    string
		Messages []map[string]string
	}
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	writeJSON(w, nil, s.Client.Publish(req.Topic, req.Messages))
}
func (s *Server) handleCreateSubscription(w http.ResponseWriter, r *http.Request) {
	var req struct{ Name, Topic string }
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	res, err := s.Client.CreateSubscription(req.Name, req.Topic)
	writeJSON(w, res, err)
}
func (s *Server) handlePull(w http.ResponseWriter, r *http.Request) {
	var req struct {
		Subscription string
		Max          int
	}
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	res, err := s.Client.Pull(req.Subscription, req.Max)
	writeJSON(w, res, err)
}
func (s *Server) handleCallFunction(w http.ResponseWriter, r *http.Request) {
	var req struct{ Name, Data string }
	if err := readJSON(r, &req); err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	res, err := s.Client.CallFunction(req.Name, req.Data)
	writeJSON(w, res, err)
}
