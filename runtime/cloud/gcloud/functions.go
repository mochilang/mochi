package gcloud

// CallFunction invokes a deployed Cloud Function.
func (c Client) CallFunction(name string, data string) (any, error) {
	url := c.ServiceEndpoint("functions", "/v1/projects/"+c.Project+"/locations/"+c.Region+"/functions/"+name+":call")
	body := map[string]any{"data": data}
	var resp any
	err := c.fetch("POST", url, mergeHeaders(c.AuthHeader(), map[string]string{"Content-Type": "application/json"}), body, &resp)
	return resp, err
}
