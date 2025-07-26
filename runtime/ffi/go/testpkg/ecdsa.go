package testpkg

import (
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/sha256"
)

func ECDSASample() map[string]any {
	priv, _ := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	msg := []byte("Rosetta Code")
	hash := sha256.Sum256(msg)
	r, s, _ := ecdsa.Sign(rand.Reader, priv, hash[:])
	ok := ecdsa.Verify(&priv.PublicKey, hash[:], r, s)
	return map[string]any{
		"D":     priv.D.String(),
		"X":     priv.PublicKey.X.String(),
		"Y":     priv.PublicKey.Y.String(),
		"R":     r.String(),
		"S":     s.String(),
		"Valid": ok,
	}
}
