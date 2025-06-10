package gocode

import "sort"

// Runtime helper functions injected into generated programs.
const (
	helperIndex = "func _index(v any, k any) any {\n" +
		"    switch s := v.(type) {\n" +
		"    case []any:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []int:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []float64:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []string:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case []bool:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid list index\")\n" +
		"        }\n" +
		"        if i < 0 {\n" +
		"            i += len(s)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(s) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return s[i]\n" +
		"    case string:\n" +
		"        i, ok := k.(int)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid string index\")\n" +
		"        }\n" +
		"        runes := []rune(s)\n" +
		"        if i < 0 {\n" +
		"            i += len(runes)\n" +
		"        }\n" +
		"        if i < 0 || i >= len(runes) {\n" +
		"            panic(\"index out of range\")\n" +
		"        }\n" +
		"        return string(runes[i])\n" +
		"    case map[string]any:\n" +
		"        ks, ok := k.(string)\n" +
		"        if !ok {\n" +
		"            panic(\"invalid map key\")\n" +
		"        }\n" +
		"        return s[ks]\n" +
		"    default:\n" +
		"        panic(\"invalid index target\")\n" +
		"    }\n" +
		"}\n"

	helperIndexString = "func _indexString(s string, i int) string {\n" +
		"    runes := []rune(s)\n" +
		"    if i < 0 {\n" +
		"        i += len(runes)\n" +
		"    }\n" +
		"    if i < 0 || i >= len(runes) {\n" +
		"        panic(\"index out of range\")\n" +
		"    }\n" +
		"    return string(runes[i])\n" +
		"}\n"

	helperIter = "func _iter(v any) []any {\n" +
		"    switch s := v.(type) {\n" +
		"    case []any:\n" +
		"        return s\n" +
		"    case []int:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case []float64:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case []string:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case []bool:\n" +
		"        out := make([]any, len(s))\n" +
		"        for i, v := range s {\n" +
		"            out[i] = v\n" +
		"        }\n" +
		"        return out\n" +
		"    case map[string]any:\n" +
		"        out := make([]any, 0, len(s))\n" +
		"        for k := range s {\n" +
		"            out = append(out, k)\n" +
		"        }\n" +
		"        return out\n" +
		"    case string:\n" +
		"        runes := []rune(s)\n" +
		"        out := make([]any, len(runes))\n" +
		"        for i, r := range runes {\n" +
		"            out[i] = string(r)\n" +
		"        }\n" +
		"        return out\n" +
		"    default:\n" +
		"        return nil\n" +
		"    }\n" +
		"}\n"

	helperGenText = "func _genText(prompt string, model string, params map[string]any) string {\n" +
		"    opts := []llm.Option{}\n" +
		"    if model != \"\" { opts = append(opts, llm.WithModel(model)) }\n" +
		"    for k, v := range params { opts = append(opts, llm.WithParam(k, v)) }\n" +
		"    resp, err := llm.Chat(context.Background(), []llm.Message{{Role: \"user\", Content: prompt}}, opts...)\n" +
		"    if err != nil { panic(err) }\n" +
		"    return resp.Message.Content\n" +
		"}\n"

	helperGenEmbed = "func _genEmbed(text string, model string, params map[string]any) []float64 {\n" +
		"    opts := []llm.EmbedOption{}\n" +
		"    if model != \"\" { opts = append(opts, llm.WithEmbedModel(model)) }\n" +
		"    for k, v := range params { opts = append(opts, llm.WithEmbedParam(k, v)) }\n" +
		"    resp, err := llm.Embed(context.Background(), text, opts...)\n" +
		"    if err != nil { panic(err) }\n" +
		"    return resp.Vector\n" +
		"}\n"

	helperGenStruct = "func _genStruct[T any](prompt string, model string, params map[string]any) T {\n" +
		"    opts := []llm.Option{}\n" +
		"    if model != \"\" { opts = append(opts, llm.WithModel(model)) }\n" +
		"    for k, v := range params { opts = append(opts, llm.WithParam(k, v)) }\n" +
		"    resp, err := llm.Chat(context.Background(), []llm.Message{{Role: \"user\", Content: prompt}}, opts...)\n" +
		"    if err != nil { panic(err) }\n" +
		"    var out T\n" +
		"    if err := json.Unmarshal([]byte(resp.Message.Content), &out); err != nil { panic(err) }\n" +
		"    return out\n" +
		"}\n"

	helperFetch = "func _fetch(url string, opts map[string]any) any {\n" +
		"    method := \"GET\"\n" +
		"    if opts != nil {\n" +
		"        if m, ok := opts[\"method\"].(string); ok {\n" +
		"            method = m\n" +
		"        }\n" +
		"    }\n" +
		"    var body io.Reader\n" +
		"    if opts != nil {\n" +
		"        if b, ok := opts[\"body\"]; ok {\n" +
		"            data, err := json.Marshal(b)\n" +
		"            if err != nil { panic(err) }\n" +
		"            body = bytes.NewReader(data)\n" +
		"        }\n" +
		"    }\n" +
		"    u, err := url.Parse(url)\n" +
		"    if err != nil { panic(err) }\n" +
		"    if opts != nil {\n" +
		"        if q, ok := opts[\"query\"]; ok {\n" +
		"            vals := u.Query()\n" +
		"            for k, v := range _toAnyMap(q) {\n" +
		"                vals.Set(k, fmt.Sprint(v))\n" +
		"            }\n" +
		"            u.RawQuery = vals.Encode()\n" +
		"        }\n" +
		"    }\n" +
		"    req, err := http.NewRequest(method, u.String(), body)\n" +
		"    if err != nil { panic(err) }\n" +
		"    if opts != nil {\n" +
		"        if hs, ok := opts[\"headers\"]; ok {\n" +
		"            for k, v := range _toAnyMap(hs) {\n" +
		"                if s, ok := v.(string); ok {\n" +
		"                    req.Header.Set(k, s)\n" +
		"                }\n" +
		"            }\n" +
		"        }\n" +
		"    }\n" +
		"    client := http.DefaultClient\n" +
		"    if opts != nil {\n" +
		"        if t, ok := opts[\"timeout\"]; ok {\n" +
		"            switch v := t.(type) {\n" +
		"            case int:\n" +
		"                client = &http.Client{Timeout: time.Duration(v) * time.Second}\n" +
		"            case int64:\n" +
		"                client = &http.Client{Timeout: time.Duration(v) * time.Second}\n" +
		"            case float64:\n" +
		"                client = &http.Client{Timeout: time.Duration(v * float64(time.Second))}\n" +
		"            case float32:\n" +
		"                client = &http.Client{Timeout: time.Duration(float64(v) * float64(time.Second))}\n" +
		"            }\n" +
		"        }\n" +
		"    }\n" +
		"    resp, err := client.Do(req)\n" +
		"    if err != nil { panic(err) }\n" +
		"    defer resp.Body.Close()\n" +
		"    data, err := io.ReadAll(resp.Body)\n" +
		"    if err != nil { panic(err) }\n" +
		"    var out any\n" +
		"    if err := json.Unmarshal(data, &out); err != nil { panic(err) }\n" +
		"    return out\n" +
		"}\n"

	helperToAnyMap = "func _toAnyMap(m any) map[string]any {\n" +
		"    switch v := m.(type) {\n" +
		"    case map[string]any:\n" +
		"        return v\n" +
		"    case map[string]string:\n" +
		"        out := make(map[string]any, len(v))\n" +
		"        for k, vv := range v {\n" +
		"            out[k] = vv\n" +
		"        }\n" +
		"        return out\n" +
		"    default:\n" +
		"        return nil\n" +
		"    }\n" +
		"}\n"

	helperCast = "func _cast[T any](v any) T {\n" +
		"    data, err := json.Marshal(v)\n" +
		"    if err != nil { panic(err) }\n" +
		"    var out T\n" +
		"    if err := json.Unmarshal(data, &out); err != nil { panic(err) }\n" +
		"    return out\n" +
		"}\n"

	helperQuery = "type _joinSpec struct { items []any; on func(...any) bool; left bool; right bool }\n" +
		"type _queryOpts struct { selectFn func(...any) any; where func(...any) bool; sortKey func(...any) any; skip int; take int }\n" +
		"func _query(src []any, joins []_joinSpec, opts _queryOpts) []any {\n" +
		"    items := make([][]any, len(src))\n" +
		"    for i, v := range src { items[i] = []any{v} }\n" +
		"    for _, j := range joins {\n" +
		"        joined := [][]any{}\n" +
		"        if j.right && j.left {\n" +
		"            matched := make([]bool, len(j.items))\n" +
		"            for _, left := range items {\n" +
		"                m := false\n" +
		"                for ri, right := range j.items {\n" +
		"                    keep := true\n" +
		"                    if j.on != nil { args := append(append([]any(nil), left...), right); keep = j.on(args...) }\n" +
		"                    if !keep { continue }\n" +
		"                    m = true; matched[ri] = true\n" +
		"                    joined = append(joined, append(append([]any(nil), left...), right))\n" +
		"                }\n" +
		"                if !m { joined = append(joined, append(append([]any(nil), left...), nil)) }\n" +
		"            }\n" +
		"            for ri, right := range j.items {\n" +
		"                if !matched[ri] { undef := make([]any, len(items[0])); joined = append(joined, append(undef, right)) }\n" +
		"            }\n" +
		"        } else if j.right {\n" +
		"            for _, right := range j.items {\n" +
		"                m := false\n" +
		"                for _, left := range items {\n" +
		"                    keep := true\n" +
		"                    if j.on != nil { args := append(append([]any(nil), left...), right); keep = j.on(args...) }\n" +
		"                    if !keep { continue }\n" +
		"                    m = true; joined = append(joined, append(append([]any(nil), left...), right))\n" +
		"                }\n" +
		"                if !m { undef := make([]any, len(items[0])); joined = append(joined, append(undef, right)) }\n" +
		"            }\n" +
		"        } else {\n" +
		"            for _, left := range items {\n" +
		"                m := false\n" +
		"                for _, right := range j.items {\n" +
		"                    keep := true\n" +
		"                    if j.on != nil { args := append(append([]any(nil), left...), right); keep = j.on(args...) }\n" +
		"                    if !keep { continue }\n" +
		"                    m = true; joined = append(joined, append(append([]any(nil), left...), right))\n" +
		"                }\n" +
		"                if j.left && !m { joined = append(joined, append(append([]any(nil), left...), nil)) }\n" +
		"            }\n" +
		"        }\n" +
		"        items = joined\n" +
		"    }\n" +
		"    if opts.where != nil {\n" +
		"        filtered := [][]any{}\n" +
		"        for _, r := range items { if opts.where(r...) { filtered = append(filtered, r) } }\n" +
		"        items = filtered\n" +
		"    }\n" +
		"    if opts.sortKey != nil {\n" +
		"        type pair struct { item []any; key any }\n" +
		"        pairs := make([]pair, len(items))\n" +
		"        for i, it := range items { pairs[i] = pair{it, opts.sortKey(it...)} }\n" +
		"        sort.Slice(pairs, func(i, j int) bool {\n" +
		"            a, b := pairs[i].key, pairs[j].key\n" +
		"            switch av := a.(type) {\n" +
		"            case int:\n" +
		"                switch bv := b.(type) { case int: return av < bv; case float64: return float64(av) < bv }\n" +
		"            case float64:\n" +
		"                switch bv := b.(type) { case int: return av < float64(bv); case float64: return av < bv }\n" +
		"            case string:\n" +
		"                bs, _ := b.(string); return av < bs\n" +
		"            }\n" +
		"            return fmt.Sprint(a) < fmt.Sprint(b)\n" +
		"        })\n" +
		"        for i, p := range pairs { items[i] = p.item }\n" +
		"    }\n" +
		"    if opts.skip >= 0 { if opts.skip < len(items) { items = items[opts.skip:] } else { items = [][]any{} } }\n" +
		"    if opts.take >= 0 { if opts.take < len(items) { items = items[:opts.take] } }\n" +
		"    res := make([]any, len(items))\n" +
		"    for i, r := range items { res[i] = opts.selectFn(r...) }\n" +
		"    return res\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_index":       helperIndex,
	"_indexString": helperIndexString,
	"_iter":        helperIter,
	"_genText":     helperGenText,
	"_genEmbed":    helperGenEmbed,
	"_genStruct":   helperGenStruct,
	"_fetch":       helperFetch,
	"_toAnyMap":    helperToAnyMap,
	"_cast":        helperCast,
	"_query":       helperQuery,
}

func (c *Compiler) use(name string) {
	c.helpers[name] = true
}

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
		c.buf.WriteByte('\n')
	}
}
