package cloudflare

// Package cloudflare provides runtime representations of Cloudflare resources.

import (
    "context"
    "os"

    cf "github.com/cloudflare/cloudflare-go"
)

// KV represents a key-value namespace.
type KV struct {
	Name string
}

// NewKV returns a new KV namespace with the given name.
func NewKV(name string) *KV { return &KV{Name: name} }

// Bucket represents an R2 object storage bucket.
type Bucket struct {
	Name string
}

// NewBucket returns a new R2 bucket.
func NewBucket(name string) *Bucket { return &Bucket{Name: name} }

// Queue represents a Cloudflare Queue.
type Queue struct {
	Name string
}

// NewQueue returns a new queue.
func NewQueue(name string) *Queue { return &Queue{Name: name} }

// Database represents a D1 database.
type Database struct {
	Name string
}

// NewDatabase returns a new database handle.
func NewDatabase(name string) *Database { return &Database{Name: name} }

// Trigger represents a cron schedule.
type Trigger struct {
	Schedule string
}

// Every creates a new cron trigger.
func Every(cron string) Trigger { return Trigger{Schedule: cron} }

// Binding represents a resource bound to a Worker.
type Binding interface{}

type (
	// KVBinding attaches a KV store.
	KVBinding struct{ Store *KV }
	// R2Binding attaches an R2 bucket.
	R2Binding struct{ Bucket *Bucket }
	// QueueBinding attaches a Queue.
	QueueBinding struct{ Q *Queue }
	// D1Binding attaches a D1 database.
	D1Binding struct{ DB *Database }
)

// Worker defines a Cloudflare Worker along with its bindings and triggers.
type Worker struct {
	Name     string
	Code     string
	Routes   []string
	Crons    []Trigger
	Bindings []Binding
}

// NewWorker constructs a Worker from name and code path.
func NewWorker(name, code string) *Worker {
	return &Worker{Name: name, Code: code}
}

// Route adds an HTTP route.
func (w *Worker) Route(path string) {
	w.Routes = append(w.Routes, path)
}

// OnCron adds a cron trigger.
func (w *Worker) OnCron(t Trigger) {
	w.Crons = append(w.Crons, t)
}

// BindKV binds a KV store.
func (w *Worker) BindKV(store *KV) {
	w.Bindings = append(w.Bindings, KVBinding{Store: store})
}

// BindR2 binds an R2 bucket.
func (w *Worker) BindR2(bucket *Bucket) {
	w.Bindings = append(w.Bindings, R2Binding{Bucket: bucket})
}

// BindQueue binds a queue.
func (w *Worker) BindQueue(q *Queue) {
	w.Bindings = append(w.Bindings, QueueBinding{Q: q})
}

// BindD1 binds a D1 database.
func (w *Worker) BindD1(db *Database) {
        w.Bindings = append(w.Bindings, D1Binding{DB: db})
}

// Client provides methods to interact with the Cloudflare API.
type Client struct {
        api       *cf.API
        accountID string
}

// NewClient constructs a Cloudflare API client using an API token.
func NewClient(apiToken, accountID string, opts ...cf.Option) (*Client, error) {
        api, err := cf.NewWithAPIToken(apiToken, opts...)
        if err != nil {
                return nil, err
        }
        return &Client{api: api, accountID: accountID}, nil
}

// DeployWorker uploads the provided worker code using the Cloudflare API.
func (c *Client) DeployWorker(ctx context.Context, w *Worker) error {
        code, err := os.ReadFile(w.Code)
        if err != nil {
                return err
        }
        _, err = c.api.UploadWorker(ctx, cf.AccountIdentifier(c.accountID), cf.CreateWorkerParams{
                ScriptName: w.Name,
                Script:     string(code),
        })
        return err
}
