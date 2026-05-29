package attest

import (
	"encoding/base64"
	"encoding/json"
	"strings"
	"testing"
)

func buildBundleJSON(t *testing.T, stmt string, payloadType string) []byte {
	t.Helper()
	b := Bundle{
		MediaType: BundleMediaTypeV2,
		DsseEnvelope: DSSEEnvelope{
			Payload:     base64.StdEncoding.EncodeToString([]byte(stmt)),
			PayloadType: payloadType,
			Signatures: []Signature{
				{Sig: "AAAA", Keyid: "key-1"},
			},
		},
	}
	out, err := json.Marshal(b)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	return out
}

func TestParseBundleHappy(t *testing.T) {
	raw := buildBundleJSON(t, validStatementJSON(), DSSEPayloadTypeInTotoJSON)
	b, err := ParseBundle(raw)
	if err != nil {
		t.Fatalf("ParseBundle: %v", err)
	}
	if b.MediaType != BundleMediaTypeV2 {
		t.Errorf("MediaType = %q", b.MediaType)
	}
	if len(b.DsseEnvelope.Signatures) != 1 {
		t.Errorf("sigs = %d", len(b.DsseEnvelope.Signatures))
	}
}

func TestParseBundleErrors(t *testing.T) {
	cases := []struct {
		name string
		body string
		want string
	}{
		{"missing-payload", `{"mediaType":"x","dsseEnvelope":{"payloadType":"y","signatures":[{"sig":"z"}]}}`, "missing dsseEnvelope.payload"},
		{"missing-payload-type", `{"mediaType":"x","dsseEnvelope":{"payload":"YWJj","signatures":[{"sig":"z"}]}}`, "missing dsseEnvelope.payloadType"},
		{"no-signatures", `{"mediaType":"x","dsseEnvelope":{"payload":"YWJj","payloadType":"y"}}`, "no signatures"},
		{"bad-json", `not-json`, "decode bundle"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ParseBundle([]byte(tc.body))
			if err == nil {
				t.Fatal("expected error")
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Errorf("error %q does not contain %q", err.Error(), tc.want)
			}
		})
	}
}

func TestBundleStatementRoundTrip(t *testing.T) {
	raw := buildBundleJSON(t, validStatementJSON(), DSSEPayloadTypeInTotoJSON)
	b, err := ParseBundle(raw)
	if err != nil {
		t.Fatal(err)
	}
	stmt, err := b.Statement()
	if err != nil {
		t.Fatalf("Statement: %v", err)
	}
	if stmt.Type != StatementTypeV1 {
		t.Errorf("type = %q", stmt.Type)
	}
}

func TestBundleStatementPayloadTypeMismatch(t *testing.T) {
	raw := buildBundleJSON(t, validStatementJSON(), "application/json")
	b, err := ParseBundle(raw)
	if err != nil {
		t.Fatal(err)
	}
	_, err = b.Statement()
	if err == nil {
		t.Fatal("expected payload type mismatch")
	}
	if !strings.Contains(err.Error(), "payloadType") {
		t.Errorf("error = %q", err.Error())
	}
}

func TestBundleStatementBadBase64(t *testing.T) {
	b := &Bundle{DsseEnvelope: DSSEEnvelope{Payload: "!!!not-base64!!!", PayloadType: DSSEPayloadTypeInTotoJSON}}
	if _, err := b.Statement(); err == nil {
		t.Fatal("expected base64 error")
	}
}

func TestBundleStatementNilSafe(t *testing.T) {
	var b *Bundle
	if _, err := b.Statement(); err == nil {
		t.Fatal("expected nil bundle error")
	}
}
