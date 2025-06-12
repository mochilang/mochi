package goffi

import (
	"go/doc"
	"golang.org/x/tools/go/packages"
	"strings"
)

// PackageInfo contains basic details about a Go package.
type PackageInfo struct {
	Path string
	Doc  string
}

// Packages returns information about all Go packages visible in the build environment.
func Packages() ([]PackageInfo, error) {
	cfg := &packages.Config{Mode: packages.NeedName | packages.NeedFiles | packages.NeedSyntax}
	pkgs, err := packages.Load(cfg, "all")
	if err != nil {
		return nil, err
	}
	infos := make([]PackageInfo, 0, len(pkgs))
	for _, p := range pkgs {
		docPkg, err := doc.NewFromFiles(p.Fset, p.Syntax, p.PkgPath)
		if err != nil {
			// ignore packages we can't parse
			continue
		}
		infos = append(infos, PackageInfo{Path: p.PkgPath, Doc: strings.TrimSpace(docPkg.Doc)})
	}
	return infos, nil
}
