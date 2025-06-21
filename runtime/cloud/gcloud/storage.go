package gcloud

// Bucket represents a Cloud Storage bucket.
type Bucket struct{ Name string }

// CreateBucket creates a new bucket.
func (c Client) CreateBucket(name string) (Bucket, error) {
	url := c.ServiceEndpoint("storage", "/storage/v1/b?project="+c.Project)
	err := c.fetch("POST", url, mergeHeaders(c.AuthHeader(), map[string]string{"Content-Type": "application/json"}), map[string]any{"name": name}, nil)
	if err != nil {
		return Bucket{}, err
	}
	return Bucket{Name: name}, nil
}

// Upload uploads an object to the bucket.
func (c Client) Upload(bucket, object, data string) error {
	url := c.ServiceEndpoint("storage", "/upload/storage/v1/b/"+bucket+"/o?uploadType=media&name="+object)
	return c.fetch("POST", url, c.AuthHeader(), data, nil)
}

// Download retrieves an object from the bucket.
func (c Client) Download(bucket, object string) (string, error) {
	url := c.ServiceEndpoint("storage", "/storage/v1/b/"+bucket+"/o/"+object+"?alt=media")
	var out string
	err := c.fetch("GET", url, c.AuthHeader(), nil, &out)
	return out, err
}

// Delete removes an object from the bucket.
func (c Client) Delete(bucket, object string) error {
	url := c.ServiceEndpoint("storage", "/storage/v1/b/"+bucket+"/o/"+object)
	return c.fetch("DELETE", url, c.AuthHeader(), nil, nil)
}

func mergeHeaders(a, b map[string]string) map[string]string {
	if len(b) == 0 {
		return a
	}
	out := make(map[string]string, len(a)+len(b))
	for k, v := range a {
		out[k] = v
	}
	for k, v := range b {
		out[k] = v
	}
	return out
}
