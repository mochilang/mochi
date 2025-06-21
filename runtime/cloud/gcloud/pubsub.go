package gcloud

// Topic represents a Pub/Sub topic.
type Topic struct{ Name string }

// Subscription represents a Pub/Sub subscription.
type Subscription struct{ Name string }

// CreateTopic creates a topic.
func (c Client) CreateTopic(name string) (Topic, error) {
	url := c.ServiceEndpoint("pubsub", "/v1/projects/"+c.Project+"/topics/"+name)
	err := c.fetch("PUT", url, c.AuthHeader(), nil, nil)
	if err != nil {
		return Topic{}, err
	}
	return Topic{Name: name}, nil
}

// Publish publishes messages to a topic.
func (c Client) Publish(topic string, messages []map[string]string) error {
	url := c.ServiceEndpoint("pubsub", "/v1/projects/"+c.Project+"/topics/"+topic+":publish")
	body := map[string]any{"messages": messages}
	return c.fetch("POST", url, mergeHeaders(c.AuthHeader(), map[string]string{"Content-Type": "application/json"}), body, nil)
}

// CreateSubscription creates a subscription to a topic.
func (c Client) CreateSubscription(name, topic string) (Subscription, error) {
	url := c.ServiceEndpoint("pubsub", "/v1/projects/"+c.Project+"/subscriptions/"+name)
	body := map[string]any{"topic": "projects/" + c.Project + "/topics/" + topic}
	err := c.fetch("PUT", url, mergeHeaders(c.AuthHeader(), map[string]string{"Content-Type": "application/json"}), body, nil)
	if err != nil {
		return Subscription{}, err
	}
	return Subscription{Name: name}, nil
}

// Pull retrieves messages from a subscription.
func (c Client) Pull(subscription string, max int) ([]any, error) {
	url := c.ServiceEndpoint("pubsub", "/v1/projects/"+c.Project+"/subscriptions/"+subscription+":pull")
	if max <= 0 {
		max = 1
	}
	body := map[string]any{"maxMessages": max}
	var resp struct {
		ReceivedMessages []any `json:"receivedMessages"`
	}
	err := c.fetch("POST", url, mergeHeaders(c.AuthHeader(), map[string]string{"Content-Type": "application/json"}), body, &resp)
	if err != nil {
		return nil, err
	}
	return resp.ReceivedMessages, nil
}
