package gcloud

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
)

// Client provides authenticated access to Google Cloud services.
type Client struct {
	Project string
	Token   string
	Region  string
}

// NewClient returns a new Client.
func NewClient(project, token, region string) Client {
	if region == "" {
		region = "us-central1"
	}
	return Client{Project: project, Token: token, Region: region}
}

// AuthHeader returns the authorization header for a client.
func (c Client) AuthHeader() map[string]string {
	return map[string]string{"Authorization": "Bearer " + c.Token}
}

// ServiceEndpoint builds the fully qualified service URL.
func (c Client) ServiceEndpoint(service, path string) string {
	host := "https://"
	switch service {
	case "storage":
		host += "storage.googleapis.com"
	case "pubsub":
		host += "pubsub.googleapis.com"
	case "functions":
		host += fmt.Sprintf("%s-cloudfunctions.googleapis.com", c.Region)
	default:
		host += service
	}
	return host + path
}

// fetch performs an HTTP request with the provided options and decodes the JSON response into out if given.
func (c Client) fetch(method, url string, headers map[string]string, body any, out any) error {
	var r io.Reader
	if body != nil {
		data, err := json.Marshal(body)
		if err != nil {
			return err
		}
		r = bytes.NewReader(data)
	}
	req, err := http.NewRequest(method, url, r)
	if err != nil {
		return err
	}
	for k, v := range headers {
		req.Header.Set(k, v)
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if out != nil {
		return json.NewDecoder(resp.Body).Decode(out)
	}
	io.Copy(io.Discard, resp.Body)
	return nil
}
