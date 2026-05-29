package attest

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func TestStaticFetcherEmpty(t *testing.T) {
	var f StaticFetcher
	_, err := f.Fetch(context.Background(), WheelTarget{})
	if !errors.Is(err, ErrNoAttestation) {
		t.Errorf("err = %v, want ErrNoAttestation", err)
	}
}

func TestStaticFetcherRoundTrip(t *testing.T) {
	payload := []byte("hello")
	f := StaticFetcher{Bundle: payload}
	got, err := f.Fetch(context.Background(), WheelTarget{})
	if err != nil {
		t.Fatalf("Fetch: %v", err)
	}
	if string(got) != "hello" {
		t.Errorf("got %q", string(got))
	}
}

func TestHTTPFetcherAttestationURL(t *testing.T) {
	tgt := WheelTarget{URL: "https://pypi.example/demo-1.0-py3-none-any.whl"}
	url, err := HTTPFetcher{}.AttestationURL(tgt)
	if err != nil {
		t.Fatalf("AttestationURL: %v", err)
	}
	want := "https://pypi.example/demo-1.0-py3-none-any.whl.provenance"
	if url != want {
		t.Errorf("url = %q, want %q", url, want)
	}
}

func TestHTTPFetcherAttestationURLNonWheel(t *testing.T) {
	tgt := WheelTarget{URL: "https://pypi.example/demo-1.0.tar.gz"}
	_, err := HTTPFetcher{}.AttestationURL(tgt)
	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), ".whl") {
		t.Errorf("error = %q", err.Error())
	}
}

func TestHTTPFetcherFetchDeferred(t *testing.T) {
	_, err := HTTPFetcher{}.Fetch(context.Background(), WheelTarget{})
	if !errors.Is(err, ErrNoAttestation) {
		t.Errorf("err = %v, want ErrNoAttestation (15.2 stub)", err)
	}
}
