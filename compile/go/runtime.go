package gocode

import "sort"

// Runtime helper functions injected into generated programs.
const (
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

	helperSliceString = "func _sliceString(s string, i, j int) string {\n" +
		"    start := i\n" +
		"    end := j\n" +
		"    n := len([]rune(s))\n" +
		"    if start < 0 { start += n }\n" +
		"    if end < 0 { end += n }\n" +
		"    if start < 0 { start = 0 }\n" +
		"    if end > n { end = n }\n" +
		"    if end < start { end = start }\n" +
		"    return string([]rune(s)[start:end])\n" +
		"}\n"
	helperCount = "func _count(v any) int {\n" +
		"    if g, ok := v.(*data.Group); ok { return len(g.Items) }\n" +
		"    switch s := v.(type) {\n" +
		"    case []any: return len(s)\n" +
		"    case []int: return len(s)\n" +
		"    case []float64: return len(s)\n" +
		"    case []string: return len(s)\n" +
		"    case []bool: return len(s)\n" +
		"    case []map[string]any: return len(s)\n" +
		"    case map[string]any: return len(s)\n" +
		"    case string: return len([]rune(s))\n" +
		"    }\n" +
		"    rv := reflect.ValueOf(v)\n" +
		"    if rv.Kind() == reflect.Slice || rv.Kind() == reflect.Array { return rv.Len() }\n" +
		"    panic(\"count() expects list or group\")\n" +
		"}\n"

	helperExists = "func _exists(v any) bool {\n" +
		"    if g, ok := v.(*data.Group); ok { return len(g.Items) > 0 }\n" +
		"    switch s := v.(type) {\n" +
		"    case []any: return len(s) > 0\n" +
		"    case []int: return len(s) > 0\n" +
		"    case []float64: return len(s) > 0\n" +
		"    case []string: return len(s) > 0\n" +
		"    case []bool: return len(s) > 0\n" +
		"    case []map[string]any: return len(s) > 0\n" +
		"    case map[string]any: return len(s) > 0\n" +
		"    case string: return len([]rune(s)) > 0\n" +
		"    }\n" +
		"    rv := reflect.ValueOf(v)\n" +
		"    if rv.Kind() == reflect.Slice || rv.Kind() == reflect.Array { return rv.Len() > 0 }\n" +
		"    return false\n" +
		"}\n"

	helperAvg = "func _avg(v any) float64 {\n" +
		"    var items []any\n" +
		"    if g, ok := v.(*data.Group); ok { items = g.Items } else {\n" +
		"        switch s := v.(type) {\n" +
		"        case []any:\n" +
		"            items = s\n" +
		"        case []int:\n" +
		"            items = []any{}\n" +
		"            for _, v := range s { items = append(items, v) }\n" +
		"        case []float64:\n" +
		"            items = []any{}\n" +
		"            for _, v := range s { items = append(items, v) }\n" +
		"        case []string:\n" +
		"            items = []any{}\n" +
		"            for _, v := range s { items = append(items, v) }\n" +
		"        case []bool:\n" +
		"            items = []any{}\n" +
		"            for _, v := range s { items = append(items, v) }\n" +
		"        default:\n" +
		"            panic(\"avg() expects list or group\")\n" +
		"        }\n" +
		"    }\n" +
		"    if len(items) == 0 { return 0 }\n" +
		"    var sum float64\n" +
		"    for _, it := range items {\n" +
		"        switch n := it.(type) {\n" +
		"        case int: sum += float64(n)\n" +
		"        case int64: sum += float64(n)\n" +
		"        case float64: sum += n\n" +
		"        default: panic(\"avg() expects numbers\") }\n" +
		"    }\n" +
		"    return sum / float64(len(items))\n" +
		"}\n"

	helperSum = "func _sum(v any) float64 {\n" +
		"    var items []any\n" +
		"    if g, ok := v.(*data.Group); ok { items = g.Items } else {\n" +
		"        switch s := v.(type) {\n" +
		"        case []any:\n" +
		"            items = s\n" +
		"        case []int:\n" +
		"            items = []any{}\n" +
		"            for _, v := range s { items = append(items, v) }\n" +
		"        case []float64:\n" +
		"            items = []any{}\n" +
		"            for _, v := range s { items = append(items, v) }\n" +
		"        case []string, []bool:\n" +
		"            panic(\"sum() expects numbers\")\n" +
		"        default:\n" +
		"            panic(\"sum() expects list or group\")\n" +
		"        }\n" +
		"    }\n" +
		"    var sum float64\n" +
		"    for _, it := range items {\n" +
		"        switch n := it.(type) {\n" +
		"        case int: sum += float64(n)\n" +
		"        case int64: sum += float64(n)\n" +
		"        case float64: sum += n\n" +
		"        default: panic(\"sum() expects numbers\") }\n" +
		"    }\n" +
		"    return sum\n" +
		"}\n"

	helperMin = "func _min(v any) any {\n" +
		"    if g, ok := v.(*data.Group); ok { v = g.Items }\n" +
		"    switch s := v.(type) {\n" +
		"    case []int:\n" +
		"        if len(s) == 0 { return 0 }\n" +
		"        m := s[0]\n" +
		"        for _, n := range s[1:] { if n < m { m = n } }\n" +
		"        return m\n" +
		"    case []float64:\n" +
		"        if len(s) == 0 { return 0.0 }\n" +
		"        m := s[0]\n" +
		"        for _, n := range s[1:] { if n < m { m = n } }\n" +
		"        return m\n" +
		"    case []string:\n" +
		"        if len(s) == 0 { return \"\" }\n" +
		"        m := s[0]\n" +
		"        for _, n := range s[1:] { if n < m { m = n } }\n" +
		"        return m\n" +
		"    case []any:\n" +
		"        if len(s) == 0 { return 0 }\n" +
		"        switch s[0].(type) {\n" +
		"        case string:\n" +
		"            m := s[0].(string)\n" +
		"            for _, it := range s[1:] { v := it.(string); if v < m { m = v } }\n" +
		"            return m\n" +
		"        case int, int64, float64:\n" +
		"            var m float64\n" +
		"            var isFloat bool\n" +
		"            switch n := s[0].(type) {\n" +
		"            case int: m = float64(n)\n" +
		"            case int64: m = float64(n)\n" +
		"            case float64: m = n; isFloat = true\n" +
		"            }\n" +
		"            for _, it := range s[1:] {\n" +
		"                switch v := it.(type) {\n" +
		"                case int: if float64(v) < m { m = float64(v) }\n" +
		"                case int64: if float64(v) < m { m = float64(v) }\n" +
		"                case float64: if v < m { m = v }; isFloat = true\n" +
		"                }\n" +
		"            }\n" +
		"            if isFloat { return m }\n" +
		"            return int(m)\n" +
		"        default:\n" +
		"            panic(\"min() expects numbers or strings\")\n" +
		"        }\n" +
		"    default:\n" +
		"        rv := reflect.ValueOf(v)\n" +
		"        if rv.Kind() == reflect.Slice {\n" +
		"            if rv.Len() == 0 { return 0 }\n" +
		"            m := rv.Index(0).Interface()\n" +
		"            switch m.(type) {\n" +
		"            case int, int64, float64:\n" +
		"                items := make([]any, rv.Len())\n" +
		"                for i := 0; i < rv.Len(); i++ { items[i] = rv.Index(i).Interface() }\n" +
		"                return _min(items)\n" +
		"            }\n" +
		"        }\n" +
		"        panic(\"min() expects list or group\")\n" +
		"    }\n" +
		"}\n"

	helperMax = "func _max(v any) any {\n" +
		"    if g, ok := v.(*data.Group); ok { v = g.Items }\n" +
		"    switch s := v.(type) {\n" +
		"    case []int:\n" +
		"        if len(s) == 0 { return 0 }\n" +
		"        m := s[0]\n" +
		"        for _, n := range s[1:] { if n > m { m = n } }\n" +
		"        return m\n" +
		"    case []float64:\n" +
		"        if len(s) == 0 { return 0.0 }\n" +
		"        m := s[0]\n" +
		"        for _, n := range s[1:] { if n > m { m = n } }\n" +
		"        return m\n" +
		"    case []any:\n" +
		"        if len(s) == 0 { return 0 }\n" +
		"        var m float64\n" +
		"        var isFloat bool\n" +
		"        switch n := s[0].(type) {\n" +
		"        case int: m = float64(n)\n" +
		"        case int64: m = float64(n)\n" +
		"        case float64: m = n; isFloat = true\n" +
		"        default: panic(\"max() expects numbers\") }\n" +
		"        for _, it := range s[1:] {\n" +
		"            switch v := it.(type) {\n" +
		"            case int: if float64(v) > m { m = float64(v) }\n" +
		"            case int64: if float64(v) > m { m = float64(v) }\n" +
		"            case float64: if v > m { m = v }; isFloat = true\n" +
		"            default: panic(\"max() expects numbers\") }\n" +
		"        }\n" +
		"        if isFloat { return m }\n" +
		"        return int(m)\n" +
		"    default:\n" +
		"        rv := reflect.ValueOf(v)\n" +
		"        if rv.Kind() == reflect.Slice {\n" +
		"            if rv.Len() == 0 { return 0 }\n" +
		"            m := rv.Index(0).Interface()\n" +
		"            switch m.(type) {\n" +
		"            case int, int64, float64:\n" +
		"                items := make([]any, rv.Len())\n" +
		"                for i := 0; i < rv.Len(); i++ { items[i] = rv.Index(i).Interface() }\n" +
		"                return _max(items)\n" +
		"            }\n" +
		"        }\n" +
		"        panic(\"max() expects list or group\")\n" +
		"    }\n" +
		"}\n"

	helperMinOrdered = "func _minOrdered[T constraints.Ordered](s []T) T {\n" +
		"    if len(s) == 0 { var zero T; return zero }\n" +
		"    m := s[0]\n" +
		"    for _, v := range s[1:] { if v < m { m = v } }\n" +
		"    return m\n" +
		"}\n"

	helperMaxOrdered = "func _maxOrdered[T constraints.Ordered](s []T) T {\n" +
		"    if len(s) == 0 { var zero T; return zero }\n" +
		"    m := s[0]\n" +
		"    for _, v := range s[1:] { if v > m { m = v } }\n" +
		"    return m\n" +
		"}\n"

	helperAvgOrdered = "func _avgOrdered[T constraints.Integer | constraints.Float](s []T) float64 {\n" +
		"    if len(s) == 0 { return 0 }\n" +
		"    var sum float64\n" +
		"    for _, v := range s { sum += float64(v) }\n" +
		"    return sum / float64(len(s))\n" +
		"}\n"

	helperSumOrdered = "func _sumOrdered[T constraints.Integer | constraints.Float](s []T) float64 {\n" +
		"    var sum float64\n" +
		"    for _, v := range s { sum += float64(v) }\n" +
		"    return sum\n" +
		"}\n"

	helperFirstSlice = "func _firstSlice[T any](s []T) T {\n" +
		"    if len(s) == 0 { var zero T; return zero }\n" +
		"    return s[0]\n" +
		"}\n"

	helperFirst = "func _first(v any) any {\n" +
		"    if g, ok := v.(*data.Group); ok {\n" +
		"        if len(g.Items) == 0 { return nil }\n" +
		"        return g.Items[0]\n" +
		"    }\n" +
		"    switch s := v.(type) {\n" +
		"    case []any:\n" +
		"        if len(s) == 0 { return nil }\n" +
		"        return s[0]\n" +
		"    case []int:\n" +
		"        if len(s) == 0 { return 0 }\n" +
		"        return s[0]\n" +
		"    case []float64:\n" +
		"        if len(s) == 0 { return 0.0 }\n" +
		"        return s[0]\n" +
		"    case []string:\n" +
		"        if len(s) == 0 { return \"\" }\n" +
		"        return s[0]\n" +
		"    case []bool:\n" +
		"        if len(s) == 0 { return false }\n" +
		"        return s[0]\n" +
		"    default:\n" +
		"        rv := reflect.ValueOf(v)\n" +
		"        if rv.Kind() == reflect.Slice && rv.Len() > 0 {\n" +
		"            return rv.Index(0).Interface()\n" +
		"        }\n" +
		"        if rv.Kind() == reflect.Array && rv.Len() > 0 {\n" +
		"            return rv.Index(0).Interface()\n" +
		"        }\n" +
		"    }\n" +
		"    return nil\n" +
		"}\n"

	helperInput = "func _input() string {\n" +
		"    var s string\n" +
		"    fmt.Scanln(&s)\n" +
		"    return s\n" +
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
		"    u, err := neturl.Parse(url)\n" +
		"    if err != nil { panic(err) }\n" +
		"    if u.Scheme == \"file\" || u.Scheme == \"\" {\n" +
		"        data, err := os.ReadFile(u.Path)\n" +
		"        if err != nil { panic(err) }\n" +
		"        var out any\n" +
		"        if err := json.Unmarshal(data, &out); err != nil { panic(err) }\n" +
		"        return out\n" +
		"    }\n" +
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

	helperToAnySlice = "func _toAnySlice[T any](s []T) []any {\n" +
		"    out := []any{}\n" +
		"    for _, v := range s { out = append(out, v) }\n" +
		"    return out\n" +
		"}\n"

	helperConvSlice = "func _convSlice[T any, U any](s []T) []U {\n" +
		"    out := []U{}\n" +
		"    for _, v := range s { out = append(out, any(v).(U)) }\n" +
		"    return out\n" +
		"}\n"

	helperContains = "func _contains(c any, v any) bool {\n" +
		"    switch s := c.(type) {\n" +
		"    case string:\n" +
		"        return strings.Contains(s, fmt.Sprint(v))\n" +
		"    case map[string]any:\n" +
		"        _, ok := s[fmt.Sprint(v)]\n" +
		"        return ok\n" +
		"    }\n" +
		"    rv := reflect.ValueOf(c)\n" +
		"    if rv.Kind() == reflect.Slice || rv.Kind() == reflect.Array {\n" +
		"        for i := 0; i < rv.Len(); i++ {\n" +
		"            if _equal(rv.Index(i).Interface(), v) { return true }\n" +
		"        }\n" +
		"        return false\n" +
		"    }\n" +
		"    return false\n" +
		"}\n"

	helperUnionAll = "func _union_all[T any](a, b []T) []T {\n" +
		"    res := make([]T, 0, len(a)+len(b))\n" +
		"    res = append(res, a...)\n" +
		"    res = append(res, b...)\n" +
		"    return res\n" +
		"}\n"

	helperUnion = "func _union[T any](a, b []T) []T {\n" +
		"    res := append([]T{}, a...)\n" +
		"    for _, it := range b {\n" +
		"        found := false\n" +
		"        for _, v := range res {\n" +
		"            if _equal(v, it) { found = true; break }\n" +
		"        }\n" +
		"        if !found { res = append(res, it) }\n" +
		"    }\n" +
		"    return res\n" +
		"}\n"

	helperConcat = "func _concat[T any](a, b []T) []T {\n" +
		"    res := make([]T, 0, len(a)+len(b))\n" +
		"    res = append(res, a...)\n" +
		"    res = append(res, b...)\n" +
		"    return res\n" +
		"}\n"

	helperReverseSlice = "func _reverseSlice[T any](s []T) []T {\n" +
		"    out := append([]T{}, s...)\n" +
		"    for i, j := 0, len(out)-1; i < j; i, j = i+1, j-1 {\n" +
		"        out[i], out[j] = out[j], out[i]\n" +
		"    }\n" +
		"    return out\n" +
		"}\n"

	helperReverseString = "func _reverseString(s string) string {\n" +
		"    r := []rune(s)\n" +
		"    for i, j := 0, len(r)-1; i < j; i, j = i+1, j-1 {\n" +
		"        r[i], r[j] = r[j], r[i]\n" +
		"    }\n" +
		"    return string(r)\n" +
		"}\n"

	helperReduce = "func _reduce[T any](src []T, fn func(T, T) T, init T) T {\n" +
		"    acc := init\n" +
		"    for _, v := range src {\n" +
		"        acc = fn(acc, v)\n" +
		"    }\n" +
		"    return acc\n" +
		"}\n"

	helperLower = "func _lower(v any) string {\n" +
		"    if s, ok := v.(string); ok { return strings.ToLower(s) }\n" +
		"    return strings.ToLower(fmt.Sprint(v))\n" +
		"}\n"

	helperUpper = "func _upper(v any) string {\n" +
		"    if s, ok := v.(string); ok { return strings.ToUpper(s) }\n" +
		"    return strings.ToUpper(fmt.Sprint(v))\n" +
		"}\n"

	helperExcept = "func _except[T any](a, b []T) []T {\n" +
		"    res := []T{}\n" +
		"    for _, x := range a {\n" +
		"        keep := true\n" +
		"        for _, y := range b {\n" +
		"            if _equal(x, y) { keep = false; break }\n" +
		"        }\n" +
		"        if keep { res = append(res, x) }\n" +
		"    }\n" +
		"    return res\n" +
		"}\n"

	helperIntersect = "func _intersect[T any](a, b []T) []T {\n" +
		"    res := []T{}\n" +
		"    for _, x := range a {\n" +
		"        inB := false\n" +
		"        for _, y := range b { if _equal(x, y) { inB = true; break } }\n" +
		"        if inB {\n" +
		"            exists := false\n" +
		"            for _, r := range res { if _equal(x, r) { exists = true; break } }\n" +
		"            if !exists { res = append(res, x) }\n" +
		"        }\n" +
		"    }\n" +
		"    return res\n" +
		"}\n"

	helperCast = "func _cast[T any](v any) T {\n" +
		"    if tv, ok := v.(T); ok { return tv }\n" +
		"    var out T\n" +
		"    switch any(out).(type) {\n" +
		"    case int:\n" +
		"        switch vv := v.(type) {\n" +
		"        case int:\n" +
		"            return any(vv).(T)\n" +
		"        case float64:\n" +
		"            return any(int(vv)).(T)\n" +
		"        case float32:\n" +
		"            return any(int(vv)).(T)\n" +
		"        }\n" +
		"    case float64:\n" +
		"        switch vv := v.(type) {\n" +
		"        case int:\n" +
		"            return any(float64(vv)).(T)\n" +
		"        case float64:\n" +
		"            return any(vv).(T)\n" +
		"        case float32:\n" +
		"            return any(float64(vv)).(T)\n" +
		"        }\n" +
		"    case float32:\n" +
		"        switch vv := v.(type) {\n" +
		"        case int:\n" +
		"            return any(float32(vv)).(T)\n" +
		"        case float64:\n" +
		"            return any(float32(vv)).(T)\n" +
		"        case float32:\n" +
		"            return any(vv).(T)\n" +
		"        }\n" +
		"    }\n" +
		"    if m, ok := v.(map[any]any); ok {\n" +
		"        v = _convertMapAny(m)\n" +
		"    }\n" +
		"    data, err := json.Marshal(v)\n" +
		"    if err != nil { panic(err) }\n" +
		"    if err := json.Unmarshal(data, &out); err != nil { panic(err) }\n" +
		"    return out\n" +
		"}\n"

	helperConvertMapAny = "func _convertMapAny(m map[any]any) map[string]any {\n" +
		"    out := make(map[string]any, len(m))\n" +
		"    for k, v := range m {\n" +
		"        key := fmt.Sprint(k)\n" +
		"        if sub, ok := v.(map[any]any); ok {\n" +
		"            out[key] = _convertMapAny(sub)\n" +
		"        } else {\n" +
		"            out[key] = v\n" +
		"        }\n" +
		"    }\n" +
		"    return out\n" +
		"}\n"

	helperEqual = "func _equal(a, b any) bool {\n" +
		"    av := reflect.ValueOf(a)\n" +
		"    bv := reflect.ValueOf(b)\n" +
		"    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {\n" +
		"        if av.Len() != bv.Len() { return false }\n" +
		"        for i := 0; i < av.Len(); i++ {\n" +
		"            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }\n" +
		"        }\n" +
		"        return true\n" +
		"    }\n" +
		"    if av.Kind() == reflect.Map && bv.Kind() == reflect.Map {\n" +
		"        if av.Len() != bv.Len() { return false }\n" +
		"        for _, k := range av.MapKeys() {\n" +
		"            bvVal := bv.MapIndex(k)\n" +
		"            if !bvVal.IsValid() { return false }\n" +
		"            if !_equal(av.MapIndex(k).Interface(), bvVal.Interface()) { return false }\n" +
		"        }\n" +
		"        return true\n" +
		"    }\n" +
		"    if (av.Kind() == reflect.Int || av.Kind() == reflect.Int64 || av.Kind() == reflect.Float64) &&\n" +
		"       (bv.Kind() == reflect.Int || bv.Kind() == reflect.Int64 || bv.Kind() == reflect.Float64) {\n" +
		"        return av.Convert(reflect.TypeOf(float64(0))).Float() == bv.Convert(reflect.TypeOf(float64(0))).Float()\n" +
		"    }\n" +
		"    return reflect.DeepEqual(a, b)\n" +
		"}\n"

	helperToMapSlice = "func _toMapSlice(v any) ([]map[string]any, bool) {\n" +
		"    switch rows := v.(type) {\n" +
		"    case []map[string]any:\n" +
		"        return rows, true\n" +
		"    case []any:\n" +
		"        out := make([]map[string]any, len(rows))\n" +
		"        for i, item := range rows {\n" +
		"            m, ok := item.(map[string]any)\n" +
		"            if !ok { return nil, false }\n" +
		"            out[i] = m\n" +
		"        }\n" +
		"        return out, true\n" +
		"    }\n" +
		"    rv := reflect.ValueOf(v)\n" +
		"    if rv.Kind() == reflect.Slice {\n" +
		"        out := make([]map[string]any, rv.Len())\n" +
		"        for i := 0; i < rv.Len(); i++ {\n" +
		"            b, err := json.Marshal(rv.Index(i).Interface())\n" +
		"            if err != nil { return nil, false }\n" +
		"            var m map[string]any\n" +
		"            if err := json.Unmarshal(b, &m); err != nil { return nil, false }\n" +
		"            out[i] = m\n" +
		"        }\n" +
		"        return out, true\n" +
		"    }\n" +
		"    return nil, false\n" +
		"}\n"

	helperLoad = "func _load(path string, opts map[string]any) []map[string]any {\n" +
		"    format := \"csv\"\n" +
		"    header := false\n" +
		"    delim := ','\n" +
		"    if opts != nil {\n" +
		"        if f, ok := opts[\"format\"].(string); ok { format = f }\n" +
		"        if h, ok := opts[\"header\"].(bool); ok { header = h }\n" +
		"        if d, ok := opts[\"delimiter\"].(string); ok && len(d) > 0 { delim = rune(d[0]) }\n" +
		"    }\n" +
		"    var rows []map[string]any\n" +
		"    var err error\n" +
		"    switch format {\n" +
		"    case \"jsonl\":\n" +
		"        if path == \"\" || path == \"-\" { rows, err = data.LoadJSONLReader(os.Stdin) } else { rows, err = data.LoadJSONL(path) }\n" +
		"    case \"json\":\n" +
		"        if path == \"\" || path == \"-\" { rows, err = data.LoadJSONReader(os.Stdin) } else { rows, err = data.LoadJSON(path) }\n" +
		"    case \"yaml\":\n" +
		"        if path == \"\" || path == \"-\" { rows, err = data.LoadYAMLReader(os.Stdin) } else { rows, err = data.LoadYAML(path) }\n" +
		"    case \"tsv\":\n" +
		"        delim = '\t'\n" +
		"        fallthrough\n" +
		"    default:\n" +
		"        if path == \"\" || path == \"-\" { rows, err = data.LoadCSVReader(os.Stdin, header, delim) } else { rows, err = data.LoadCSV(path, header, delim) }\n" +
		"    }\n" +
		"    if err != nil { panic(err) }\n" +
		"    return rows\n" +
		"}\n"

	helperSave = "func _save(src any, path string, opts map[string]any) {\n" +
		"    rows, ok := _toMapSlice(src)\n" +
		"    if !ok { panic(\"save source must be list of maps\") }\n" +
		"    format := \"csv\"\n" +
		"    header := false\n" +
		"    delim := ','\n" +
		"    if opts != nil {\n" +
		"        if f, ok := opts[\"format\"].(string); ok { format = f }\n" +
		"        if h, ok := opts[\"header\"].(bool); ok { header = h }\n" +
		"        if d, ok := opts[\"delimiter\"].(string); ok && len(d) > 0 { delim = rune(d[0]) }\n" +
		"    }\n" +
		"    var err error\n" +
		"    switch format {\n" +
		"    case \"jsonl\":\n" +
		"        if path == \"\" || path == \"-\" { err = data.SaveJSONLWriter(rows, os.Stdout) } else { err = data.SaveJSONL(rows, path) }\n" +
		"    case \"json\":\n" +
		"        if path == \"\" || path == \"-\" { err = data.SaveJSONWriter(rows, os.Stdout) } else { err = data.SaveJSON(rows, path) }\n" +
		"    case \"yaml\":\n" +
		"        if path == \"\" || path == \"-\" { err = data.SaveYAMLWriter(rows, os.Stdout) } else { err = data.SaveYAML(rows, path) }\n" +
		"    case \"tsv\":\n" +
		"        delim = '\t'\n" +
		"        fallthrough\n" +
		"    default:\n" +
		"        if path == \"\" || path == \"-\" { err = data.SaveCSVWriter(rows, os.Stdout, header, delim) } else { err = data.SaveCSV(rows, path, header, delim) }\n" +
		"    }\n" +
		"    if err != nil { panic(err) }\n" +
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

	helperPaginate = "func _paginate[T any](src []T, skip, take int) []T {\n" +
		"    if skip > 0 {\n" +
		"        if skip < len(src) { src = src[skip:] } else { return []T{} }\n" +
		"    }\n" +
		"    if take >= 0 && take < len(src) { src = src[:take] }\n" +
		"    return src\n" +
		"}\n"
)

var helperMap = map[string]string{
	"_indexString":   helperIndexString,
	"_sliceString":   helperSliceString,
	"_count":         helperCount,
	"_exists":        helperExists,
	"_avg":           helperAvg,
	"_sum":           helperSum,
	"_min":           helperMin,
	"_max":           helperMax,
	"_minOrdered":    helperMinOrdered,
	"_maxOrdered":    helperMaxOrdered,
	"_avgOrdered":    helperAvgOrdered,
	"_sumOrdered":    helperSumOrdered,
	"_firstSlice":    helperFirstSlice,
	"_first":         helperFirst,
	"_input":         helperInput,
	"_genText":       helperGenText,
	"_genEmbed":      helperGenEmbed,
	"_genStruct":     helperGenStruct,
	"_fetch":         helperFetch,
	"_toAnyMap":      helperToAnyMap,
	"_toAnySlice":    helperToAnySlice,
	"_convSlice":     helperConvSlice,
	"_contains":      helperContains,
	"_union_all":     helperUnionAll,
	"_union":         helperUnion,
	"_concat":        helperConcat,
	"_reduce":        helperReduce,
	"_reverseSlice":  helperReverseSlice,
	"_reverseString": helperReverseString,
	"_lower":         helperLower,
	"_upper":         helperUpper,
	"_except":        helperExcept,
	"_intersect":     helperIntersect,
	"_cast":          helperCast,
	"_convertMapAny": helperConvertMapAny,
	"_equal":         helperEqual,
	"_query":         helperQuery,
	"_paginate":      helperPaginate,
	"_load":          helperLoad,
	"_save":          helperSave,
	"_toMapSlice":    helperToMapSlice,
}

func (c *Compiler) use(name string) {
	c.helpers[name] = true
	if name == "_cast" {
		c.imports["encoding/json"] = true
		c.imports["fmt"] = true
		c.helpers["_convertMapAny"] = true
	}
	if name == "_toMapSlice" {
		c.imports["encoding/json"] = true
		c.imports["reflect"] = true
	}
	if name == "_lower" || name == "_upper" {
		c.imports["fmt"] = true
		c.imports["strings"] = true
	}
	if name == "_contains" {
		c.imports["fmt"] = true
		c.imports["strings"] = true
		c.imports["reflect"] = true
		c.helpers["_equal"] = true
	}
	if name == "_minOrdered" || name == "_maxOrdered" || name == "_avgOrdered" || name == "_sumOrdered" {
		c.imports["golang.org/x/exp/constraints"] = true
	}
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
