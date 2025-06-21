package ccode

// Runtime helper functions injected into generated C programs.

const (
	helperListInt = `typedef struct { int len; int *data; } list_int;
static list_int list_int_create(int len) {
    list_int l;
    l.len = len;
    l.data = (int*)malloc(sizeof(int)*len);
    return l;
}
`
	helperListFloat = `typedef struct { int len; double *data; } list_float;
static list_float list_float_create(int len) {
    list_float l;
    l.len = len;
    l.data = (double*)malloc(sizeof(double)*len);
    return l;
}
`
	helperListString = `typedef struct { int len; char** data; } list_string;
static list_string list_string_create(int len) {
    list_string l;
    l.len = len;
    l.data = (char**)malloc(sizeof(char*)*len);
    return l;
}
`
	helperListListInt = `typedef struct { int len; list_int *data; } list_list_int;
static list_list_int list_list_int_create(int len) {
    list_list_int l;
    l.len = len;
    l.data = (list_int*)malloc(sizeof(list_int)*len);
    return l;
}
`
	helperConcatListInt = `static list_int concat_list_int(list_int a, list_int b) {
    list_int r = list_int_create(a.len + b.len);
    for (int i = 0; i < a.len; i++) r.data[i] = a.data[i];
    for (int i = 0; i < b.len; i++) r.data[a.len + i] = b.data[i];
    return r;
}
`
	helperConcatListFloat = `static list_float concat_list_float(list_float a, list_float b) {
    list_float r = list_float_create(a.len + b.len);
    for (int i = 0; i < a.len; i++) r.data[i] = a.data[i];
    for (int i = 0; i < b.len; i++) r.data[a.len + i] = b.data[i];
    return r;
}
`
	helperConcatListString = `static list_string concat_list_string(list_string a, list_string b) {
    list_string r = list_string_create(a.len + b.len);
    for (int i = 0; i < a.len; i++) r.data[i] = a.data[i];
    for (int i = 0; i < b.len; i++) r.data[a.len + i] = b.data[i];
    return r;
}
`
	helperConcatListListInt = `static list_list_int concat_list_list_int(list_list_int a, list_list_int b) {
    list_list_int r = list_list_int_create(a.len + b.len);
    for (int i = 0; i < a.len; i++) r.data[i] = a.data[i];
    for (int i = 0; i < b.len; i++) r.data[a.len + i] = b.data[i];
    return r;
}
`
	helperConcatString = `static char* concat_string(char* a, char* b) {
    size_t len1 = strlen(a);
    size_t len2 = strlen(b);
    char* buf = (char*)malloc(len1 + len2 + 1);
    memcpy(buf, a, len1);
    memcpy(buf + len1, b, len2);
    buf[len1 + len2] = '\0';
    return buf;
}
`
	helperUnionListInt = `static list_int union_list_int(list_int a, list_int b) {
    list_int r = list_int_create(a.len + b.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (r.data[j] == a.data[i]) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    for (int i = 0; i < b.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (r.data[j] == b.data[i]) { found = 1; break; }
        if (!found) r.data[idx++] = b.data[i];
    }
    r.len = idx;
    return r;
}
`
	helperUnionListFloat = `static list_float union_list_float(list_float a, list_float b) {
    list_float r = list_float_create(a.len + b.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (r.data[j] == a.data[i]) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    for (int i = 0; i < b.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (r.data[j] == b.data[i]) { found = 1; break; }
        if (!found) r.data[idx++] = b.data[i];
    }
    r.len = idx;
    return r;
}
`
	helperUnionListString = `static list_string union_list_string(list_string a, list_string b) {
    list_string r = list_string_create(a.len + b.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (strcmp(r.data[j], a.data[i]) == 0) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    for (int i = 0; i < b.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (strcmp(r.data[j], b.data[i]) == 0) { found = 1; break; }
        if (!found) r.data[idx++] = b.data[i];
    }
    r.len = idx;
    return r;
}
`
	helperExceptListInt = `static list_int except_list_int(list_int a, list_int b) {
    list_int r = list_int_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (a.data[i] == b.data[j]) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    r.len = idx;
    return r;
}
`
	helperExceptListFloat = `static list_float except_list_float(list_float a, list_float b) {
    list_float r = list_float_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (a.data[i] == b.data[j]) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    r.len = idx;
    return r;
}
`
	helperExceptListString = `static list_string except_list_string(list_string a, list_string b) {
    list_string r = list_string_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (strcmp(a.data[i], b.data[j]) == 0) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    r.len = idx;
    return r;
}
`
	helperIntersectListInt = `static list_int intersect_list_int(list_int a, list_int b) {
    list_int r = list_int_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (a.data[i] == b.data[j]) { found = 1; break; }
        if (found) {
            int dup = 0;
            for (int j = 0; j < idx; j++) if (r.data[j] == a.data[i]) { dup = 1; break; }
            if (!dup) r.data[idx++] = a.data[i];
        }
    }
    r.len = idx;
    return r;
}
`
	helperIntersectListFloat = `static list_float intersect_list_float(list_float a, list_float b) {
    list_float r = list_float_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (a.data[i] == b.data[j]) { found = 1; break; }
        if (found) {
            int dup = 0;
            for (int j = 0; j < idx; j++) if (r.data[j] == a.data[i]) { dup = 1; break; }
            if (!dup) r.data[idx++] = a.data[i];
        }
    }
    r.len = idx;
    return r;
}
`
	helperIntersectListString = `static list_string intersect_list_string(list_string a, list_string b) {
    list_string r = list_string_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (strcmp(a.data[i], b.data[j]) == 0) { found = 1; break; }
        if (found) {
            int dup = 0;
            for (int j = 0; j < idx; j++) if (strcmp(r.data[j], a.data[i]) == 0) { dup = 1; break; }
            if (!dup) r.data[idx++] = a.data[i];
        }
    }
    r.len = idx;
    return r;
}
`
	helperCount = `static int _count(list_int v) { return v.len; }
`
	helperAvg = `static double _avg(list_int v) {
    if (v.len == 0) return 0;
    double sum = 0;
    for (int i = 0; i < v.len; i++) sum += v.data[i];
    return sum / v.len;
}
`
	helperContainsListInt = `static int contains_list_int(list_int v, int item) {
    for (int i = 0; i < v.len; i++) if (v.data[i] == item) return 1;
    return 0;
}
`
	helperContainsListFloat = `static int contains_list_float(list_float v, double item) {
    for (int i = 0; i < v.len; i++) if (v.data[i] == item) return 1;
    return 0;
}
`
	helperContainsListString = `static int contains_list_string(list_string v, char* item) {
    for (int i = 0; i < v.len; i++) if (strcmp(v.data[i], item) == 0) return 1;
    return 0;
}
`
	helperInput = `static char* _input() {
    char buf[1024];
    if (!fgets(buf, sizeof(buf), stdin)) return strdup("");
    size_t len = strlen(buf);
    if (len > 0 && buf[len-1] == '\n') buf[len-1] = '\0';
    return strdup(buf);
}
`
	helperStr = `static char* _str(int v) {
    char* buf = (char*)malloc(32);
    sprintf(buf, "%d", v);
    return buf;
}
`
	helperNow = `static long long _now() {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (long long)ts.tv_sec * 1000000000LL + ts.tv_nsec;
}
`
	helperJSON = `static void _json_int(int v) { printf("%d", v); }
static void _json_float(double v) { printf("%g", v); }
static void _json_string(char* s) { printf("\"%s\"", s); }
static void _json_list_int(list_int v) {
    printf("[");
    for (int i = 0; i < v.len; i++) { if (i > 0) printf(","); _json_int(v.data[i]); }
    printf("]");
}
static void _json_list_float(list_float v) {
    printf("[");
    for (int i = 0; i < v.len; i++) { if (i > 0) printf(","); _json_float(v.data[i]); }
    printf("]");
}
static void _json_list_string(list_string v) {
    printf("[");
    for (int i = 0; i < v.len; i++) { if (i > 0) printf(","); _json_string(v.data[i]); }
    printf("]");
}
static void _json_list_list_int(list_list_int v) {
    printf("[");
    for (int i = 0; i < v.len; i++) { if (i > 0) printf(","); _json_list_int(v.data[i]); }
    printf("]");
}
`
	helperIndexString = `static char* _index_string(char* s, int i) {
    int len = strlen(s);
    if (i < 0) i += len;
    if (i < 0 || i >= len) { fprintf(stderr, "index out of range\n"); exit(1); }
    char* buf = (char*)malloc(2);
    buf[0] = s[i];
    buf[1] = '\0';
    return buf;
}
`
	helperSliceString = `static char* slice_string(char* s, int start, int end) {
    int len = strlen(s);
    if (start < 0) start += len;
    if (end < 0) end += len;
    if (start < 0) start = 0;
    if (end > len) end = len;
    if (start > end) start = end;
    char* buf = (char*)malloc(end - start + 1);
    memcpy(buf, s + start, end - start);
    buf[end - start] = '\0';
    return buf;
}
`
	helperSliceListInt = `static list_int slice_list_int(list_int v, int start, int end) {
    if (start < 0) start += v.len;
    if (end < 0) end += v.len;
    if (start < 0) start = 0;
    if (end > v.len) end = v.len;
    if (start > end) start = end;
    list_int r = list_int_create(end - start);
    for (int i = 0; i < r.len; i++) r.data[i] = v.data[start + i];
    return r;
}
`
	helperSliceListFloat = `static list_float slice_list_float(list_float v, int start, int end) {
    if (start < 0) start += v.len;
    if (end < 0) end += v.len;
    if (start < 0) start = 0;
    if (end > v.len) end = v.len;
    if (start > end) start = end;
    list_float r = list_float_create(end - start);
    for (int i = 0; i < r.len; i++) r.data[i] = v.data[start + i];
    return r;
}
`
	helperSliceListString = `static list_string slice_list_string(list_string v, int start, int end) {
    if (start < 0) start += v.len;
    if (end < 0) end += v.len;
    if (start < 0) start = 0;
    if (end > v.len) end = v.len;
    if (start > end) start = end;
    list_string r = list_string_create(end - start);
    for (int i = 0; i < r.len; i++) r.data[i] = v.data[start + i];
    return r;
}
`
	helperPrintListInt = `static void _print_list_int(list_int v) {
    printf("[");
    for (int i = 0; i < v.len; i++) {
        if (i > 0) printf(" ");
        printf("%d", v.data[i]);
    }
    printf("]");
}
`
	helperPrintListListInt = `static void _print_list_list_int(list_list_int v) {
    printf("[");
    for (int i = 0; i < v.len; i++) {
        if (i > 0) printf(" ");
        _print_list_int(v.data[i]);
    }
    printf("]");
}
`
	helperPrintListFloat = `static void _print_list_float(list_float v) {
    printf("[");
    for (int i = 0; i < v.len; i++) {
        if (i > 0) printf(" ");
        printf("%g", v.data[i]);
    }
    printf("]");
}
`
	helperPrintListString = `static void _print_list_string(list_string v) {
    printf("[");
    for (int i = 0; i < v.len; i++) {
        if (i > 0) printf(" ");
        printf("%s", v.data[i]);
    }
    printf("]");
}
`
)

// Mapping of helper requirement keys to their C implementations and the order
// they should be emitted in.
var helperCode = map[string]string{
	needListFloat:           helperListFloat,
	needListString:          helperListString,
	needListListInt:         helperListListInt,
	needConcatListInt:       helperConcatListInt,
	needConcatListFloat:     helperConcatListFloat,
	needConcatListString:    helperConcatListString,
	needConcatListListInt:   helperConcatListListInt,
	needConcatString:        helperConcatString,
	needUnionListInt:        helperUnionListInt,
	needUnionListFloat:      helperUnionListFloat,
	needUnionListString:     helperUnionListString,
	needExceptListInt:       helperExceptListInt,
	needExceptListFloat:     helperExceptListFloat,
	needExceptListString:    helperExceptListString,
	needIntersectListInt:    helperIntersectListInt,
	needIntersectListFloat:  helperIntersectListFloat,
	needIntersectListString: helperIntersectListString,
	needCount:               helperCount,
	needAvg:                 helperAvg,
	needInListInt:           helperContainsListInt,
	needInListFloat:         helperContainsListFloat,
	needInListString:        helperContainsListString,
	needInput:               helperInput,
	needStr:                 helperStr,
	needNow:                 helperNow,
	needJSON:                helperJSON,
	needIndexString:         helperIndexString,
	needSliceString:         helperSliceString,
	needSliceListInt:        helperSliceListInt,
	needSliceListFloat:      helperSliceListFloat,
	needSliceListString:     helperSliceListString,
}

var helperOrder = []string{
	needListFloat,
	needListString,
	needListListInt,
	needConcatListInt,
	needConcatListFloat,
	needConcatListString,
	needConcatListListInt,
	needConcatString,
	needUnionListInt,
	needUnionListFloat,
	needUnionListString,
	needExceptListInt,
	needExceptListFloat,
	needExceptListString,
	needIntersectListInt,
	needIntersectListFloat,
	needIntersectListString,
	needCount,
	needAvg,
	needInListInt,
	needInListFloat,
	needInListString,
	needInput,
	needStr,
	needNow,
	needJSON,
	needIndexString,
	needSliceString,
	needSliceListInt,
	needSliceListFloat,
	needSliceListString,
}

func (c *Compiler) emitRuntime() {
	c.buf.WriteString(helperListInt)
	for _, h := range helperOrder {
		if c.has(h) {
			c.buf.WriteString(helperCode[h])
		}
	}

	// printing helpers
	c.buf.WriteString(helperPrintListInt)
	if c.has(needListListInt) {
		c.buf.WriteString(helperPrintListListInt)
	}
	if c.has(needListFloat) {
		c.buf.WriteString(helperPrintListFloat)
	}
	if c.has(needListString) {
		c.buf.WriteString(helperPrintListString)
	}
}
