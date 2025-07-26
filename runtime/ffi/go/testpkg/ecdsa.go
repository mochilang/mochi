package testpkg

import (
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/sha256"
	"encoding/binary"
	"math/big"
)

type ECDSAResult struct {
	D     string
	X     string
	Y     string
	Hash  string
	R     string
	S     string
	Valid bool
}

// ECDSAExample deterministically signs "Rosetta Code" using P-256.
func ECDSAExample() ECDSAResult {
	curve := elliptic.P256()
	priv := &ecdsa.PrivateKey{PublicKey: ecdsa.PublicKey{Curve: curve}}
	priv.D = big.NewInt(1234567890)
	priv.PublicKey.X, priv.PublicKey.Y = curve.ScalarBaseMult(priv.D.Bytes())

	msg := []byte("Rosetta Code")
	hash := sha256.Sum256(msg)

	k := big.NewInt(1234567890)
	x1, _ := curve.ScalarBaseMult(k.Bytes())
	r := new(big.Int).Mod(x1, curve.Params().N)
	kInv := new(big.Int).ModInverse(k, curve.Params().N)
	s := new(big.Int).Mul(priv.D, r)
	s.Add(s, new(big.Int).SetBytes(hash[:]))
	s.Mul(s, kInv)
	s.Mod(s, curve.Params().N)

	valid := ecdsa.Verify(&priv.PublicKey, hash[:], r, s)
	return ECDSAResult{
		D:     priv.D.String(),
		X:     priv.PublicKey.X.String(),
		Y:     priv.PublicKey.Y.String(),
		Hash:  "0x" + big.NewInt(0).SetUint64(uint64(binary.BigEndian.Uint32(hash[:]))).Text(16),
		R:     r.String(),
		S:     s.String(),
		Valid: valid,
	}
}
