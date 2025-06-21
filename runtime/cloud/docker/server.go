package docker

import (
	"encoding/json"
	"net/http"
	"os/exec"
	"strings"
)

// Handler returns an http.Handler that exposes a minimal Docker API.
// Each endpoint responds with the Docker command that would be executed.
func Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/image/build", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name, Context string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"build", "-t", req.Name, req.Context})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/image/push", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"push", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/image/remove", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"rmi", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/container/run", func(w http.ResponseWriter, r *http.Request) {
		var req struct {
			Name, Image string
			Args        []string
		}
		json.NewDecoder(r.Body).Decode(&req)
		args := append([]string{"run", "--name", req.Name, req.Image}, req.Args...)
		cmd := buildCmd(args)
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/container/stop", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"stop", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/container/remove", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"rm", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/network/create", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"network", "create", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/network/remove", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"network", "rm", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/volume/create", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"volume", "create", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	mux.HandleFunc("/volume/remove", func(w http.ResponseWriter, r *http.Request) {
		var req struct{ Name string }
		json.NewDecoder(r.Body).Decode(&req)
		cmd := buildCmd([]string{"volume", "rm", req.Name})
		json.NewEncoder(w).Encode(map[string]string{"command": cmd})
	})
	return mux
}

// buildCmd returns the docker command as a string. If docker is available,
// the command output is executed and returned; otherwise only the command is returned.
func buildCmd(args []string) string {
	if _, err := exec.LookPath("docker"); err == nil {
		out, err := exec.Command("docker", args...).CombinedOutput()
		if err == nil {
			return string(out)
		}
	}
	return strings.Join(append([]string{"docker"}, args...), " ")
}
