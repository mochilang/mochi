// Package lower - Phase 14: HTTP fetch lowering for Kotlin backend.
//
// HttpGetExpr is lowered to a call to a generated mochiHttpGet() helper function
// that uses Java's built-in java.net.http.HttpClient (available since Java 11).
//
// For tests, when MOCHI_TEST_HTTP_BASE is set, the helper uses that as the base URL.
// "HTTPTEST_URL" in the URL is replaced with the value of MOCHI_TEST_HTTP_BASE.
package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/kotlin/ktree"
)

// lowerHttpGetExpr lowers HttpGetExpr to mochiHttpGet(url).
func (l *lowerer) lowerHttpGetExpr(e *aotir.HttpGetExpr) (ktree.Expr, error) {
	url, err := l.lowerExpr(e.URL)
	if err != nil {
		return nil, err
	}
	code := fmt.Sprintf("mochiHttpGet(%s)", url.KtExprString())
	return &ktree.RawKtExpr{Code: code}, nil
}

// lowerJsonDecodeExpr lowers JsonDecodeExpr to mochiJsonDecode(input).
func (l *lowerer) lowerJsonDecodeExpr(e *aotir.JsonDecodeExpr) (ktree.Expr, error) {
	input, err := l.lowerExpr(e.Input)
	if err != nil {
		return nil, err
	}
	code := fmt.Sprintf("mochiJsonDecode(%s)", input.KtExprString())
	l.usesHTTP = true // reuse the HTTP helper flag to include json decode helper
	return &ktree.RawKtExpr{Code: code}, nil
}

// mochiHttpHelpers returns the Kotlin helper functions for HTTP and JSON.
func mochiHttpHelpers() []ktree.Decl {
	return []ktree.Decl{
		&ktree.RawKtDecl{Code: `
fun mochiHttpGet(url: String): String {
    val baseUrl = System.getenv("MOCHI_TEST_HTTP_BASE") ?: ""
    val resolvedUrl = if (baseUrl.isNotEmpty()) url.replace("HTTPTEST_URL", baseUrl) else url
    val client = java.net.http.HttpClient.newHttpClient()
    val request = java.net.http.HttpRequest.newBuilder()
        .uri(java.net.URI.create(resolvedUrl))
        .GET()
        .build()
    val response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
    return response.body()
}

fun mochiJsonDecode(s: String): LinkedHashMap<String, String> {
    val result = LinkedHashMap<String, String>()
    val trimmed = s.trim().removePrefix("{").removeSuffix("}")
    if (trimmed.isBlank()) return result
    val pairs = trimmed.split(",")
    for (pair in pairs) {
        val kv = pair.trim().split(":", limit = 2)
        if (kv.size != 2) continue
        val k = kv[0].trim().removeSurrounding("\"")
        val rawV = kv[1].trim()
        val v = if (rawV.startsWith("\"")) rawV.removeSurrounding("\"") else rawV
        result[k] = v
    }
    return result
}
`},
	}
}
