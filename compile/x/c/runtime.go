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
}`
	helperMapIntBool = `typedef struct { int key; int value; } map_int_bool_item;
static map_int_bool_item* map_int_bool_item_new(int key, int value) {
    map_int_bool_item* it = (map_int_bool_item*)malloc(sizeof(map_int_bool_item));
    it->key = key;
    it->value = value;
    return it;
}
typedef struct { int len; int cap; map_int_bool_item** data; } map_int_bool;
static map_int_bool map_int_bool_create(int cap) {
    map_int_bool m; m.len = 0; m.cap = cap;
    m.data = cap ? (map_int_bool_item**)malloc(sizeof(map_int_bool_item*)*cap) : NULL;
    return m;
}
static void map_int_bool_put(map_int_bool* m, int key, int value) {
    for (int i=0;i<m->len;i++) if (m->data[i]->key == key) { m->data[i]->value = value; return; }
    if (m->len >= m->cap) {
        m->cap = m->cap ? m->cap*2 : 4;
        m->data = (map_int_bool_item**)realloc(m->data, sizeof(map_int_bool_item*)*m->cap);
    }
    m->data[m->len++] = map_int_bool_item_new(key, value);
}
static int map_int_bool_contains(map_int_bool m, int key) {
    for (int i=0;i<m.len;i++) if (m.data[i]->key == key) return 1;
    return 0;
}`

	helperGroupByInt = `typedef struct { int key; list_int items; } _GroupInt;
typedef struct { int len; int cap; _GroupInt* data; } list_group_int;
static list_group_int _group_by_int(list_int src) {
    list_group_int res; res.len = 0; res.cap = 0; res.data = NULL;
    for (int i=0; i<src.len; i++) {
        int key = src.data[i];
        int idx = -1;
        for (int j=0; j<res.len; j++) if (res.data[j].key == key) { idx = j; break; }
        if (idx == -1) {
            if (res.len >= res.cap) { res.cap = res.cap ? res.cap*2 : 4; res.data = (_GroupInt*)realloc(res.data, sizeof(_GroupInt)*res.cap); }
            res.data[res.len].key = key;
            res.data[res.len].items = list_int_create(0);
            idx = res.len++;
        }
        _GroupInt* g = &res.data[idx];
        g->items.data = (int*)realloc(g->items.data, sizeof(int)*(g->items.len+1));
        g->items.data[g->items.len++] = src.data[i];
    }
    return res;
}`

	helperConcatListInt = `static list_int concat_list_int(list_int a, list_int b) {
    list_int r = list_int_create(a.len + b.len);
    for (int i = 0; i < a.len; i++) r.data[i] = a.data[i];
    for (int i = 0; i < b.len; i++) r.data[a.len + i] = b.data[i];
    return r;
}`
	helperEqualListInt = `static int equal_list_int(list_int a, list_int b) {
    if (a.len != b.len) return 0;
    for (int i = 0; i < a.len; i++) if (a.data[i] != b.data[i]) return 0;
    return 1;
}`

	helperConcatListFloat = `static list_float concat_list_float(list_float a, list_float b) {
    list_float r = list_float_create(a.len + b.len);
    for (int i = 0; i < a.len; i++) r.data[i] = a.data[i];
    for (int i = 0; i < b.len; i++) r.data[a.len + i] = b.data[i];
    return r;
}
`
	helperPairString     = `typedef struct { char* a; char* b; } pair_string;`
	helperListPairString = `typedef struct { int len; pair_string* data; } list_pair_string;
static list_pair_string list_pair_string_create(int len) {
    list_pair_string l; l.len = len; l.data = (pair_string*)malloc(sizeof(pair_string)*len); return l;
}`
	helperGroupByPairString = `typedef struct { pair_string key; list_int items; } _GroupPairString;
typedef struct { int len; int cap; _GroupPairString* data; } list_group_pair_string;
static list_group_pair_string _group_by_pair_string(list_pair_string src) {
    list_group_pair_string res; res.len = 0; res.cap = 0; res.data = NULL;
    for (int i=0; i<src.len; i++) {
        pair_string key = src.data[i];
        int idx = -1;
        for (int j=0; j<res.len; j++) if (strcmp(res.data[j].key.a, key.a)==0 && strcmp(res.data[j].key.b, key.b)==0) { idx = j; break; }
        if (idx == -1) {
            if (res.len >= res.cap) { res.cap = res.cap ? res.cap*2 : 4; res.data = (_GroupPairString*)realloc(res.data, sizeof(_GroupPairString)*res.cap); }
            res.data[res.len].key = key;
            res.data[res.len].items = list_int_create(0);
            idx = res.len++;
        }
        _GroupPairString* g = &res.data[idx];
        g->items.data = (int*)realloc(g->items.data, sizeof(int)*(g->items.len+1));
        g->items.data[g->items.len++] = i;
    }
    return res;
}`
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
	helperUnionListListInt = `static list_list_int union_list_list_int(list_list_int a, list_list_int b) {
    list_list_int r = list_list_int_create(a.len + b.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (equal_list_int(r.data[j], a.data[i])) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    for (int i = 0; i < b.len; i++) {
        int found = 0;
        for (int j = 0; j < idx; j++) if (equal_list_int(r.data[j], b.data[i])) { found = 1; break; }
        if (!found) r.data[idx++] = b.data[i];
    }
    r.len = idx;
    return r;
}`
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
	helperExceptListListInt = `static list_list_int except_list_list_int(list_list_int a, list_list_int b) {
    list_list_int r = list_list_int_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (equal_list_int(a.data[i], b.data[j])) { found = 1; break; }
        if (!found) r.data[idx++] = a.data[i];
    }
    r.len = idx;
    return r;
}`
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
	helperIntersectListListInt = `static list_list_int intersect_list_list_int(list_list_int a, list_list_int b) {
    list_list_int r = list_list_int_create(a.len);
    int idx = 0;
    for (int i = 0; i < a.len; i++) {
        int found = 0;
        for (int j = 0; j < b.len; j++) if (equal_list_int(a.data[i], b.data[j])) { found = 1; break; }
        if (found) {
            int dup = 0;
            for (int j = 0; j < idx; j++) if (equal_list_int(r.data[j], a.data[i])) { dup = 1; break; }
            if (!dup) r.data[idx++] = a.data[i];
        }
    }
    r.len = idx;
    return r;
}`
	helperCount = `static int _count(list_int v) { return v.len; }
`
	helperSumInt = `static int _sum_int(list_int v) {
    int sum = 0;
    for (int i = 0; i < v.len; i++) sum += v.data[i];
    return sum;
}`
	helperSumFloat = `static double _sum_float(list_float v) {
    double sum = 0;
    for (int i = 0; i < v.len; i++) sum += v.data[i];
    return sum;
}`
	helperAvg = `static double _avg(list_int v) {
    if (v.len == 0) return 0;
    double sum = 0;
    for (int i = 0; i < v.len; i++) sum += v.data[i];
    return sum / v.len;
}`
	helperAvgFloat = `static double _avg_float(list_float v) {
    if (v.len == 0) return 0;
    double sum = 0;
    for (int i = 0; i < v.len; i++) sum += v.data[i];
    return sum / v.len;
}`
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
	helperContainsString = `static int contains_string(char* s, char* sub) {
    return strstr(s, sub) != NULL;
}`
	helperContainsListListInt = `static int contains_list_list_int(list_list_int v, list_int item) {
    for (int i = 0; i < v.len; i++) if (equal_list_int(v.data[i], item)) return 1;
    return 0;
}`
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
	helperLoadJSON = `static char* _read_all(const char* path) {
    FILE* f = (!path || path[0]=='\0' || strcmp(path,"-")==0) ? stdin : fopen(path, "r");
    if (!f) { fprintf(stderr, "cannot open %s\n", path); exit(1); }
    fseek(f,0,SEEK_END); long n = ftell(f); if (n<0) n = 0; rewind(f);
    char* buf = (char*)malloc(n+1); fread(buf,1,n,f); buf[n]='\0';
    if (f!=stdin) fclose(f); return buf; }
typedef struct { char* key; char* value; } pair_string;
typedef struct { int len; int cap; pair_string* data; } map_string;
static map_string map_string_create(int cap){ map_string m; m.len=0; m.cap=cap; m.data=cap?(pair_string*)malloc(sizeof(pair_string)*cap):NULL; return m; }
static void map_string_put(map_string* m,char* k,char* v){ if(m->len>=m->cap){ m->cap=m->cap?m->cap*2:4; m->data=(pair_string*)realloc(m->data,sizeof(pair_string)*m->cap);} m->data[m->len].key=k; m->data[m->len].value=v; m->len++; }
typedef struct { int len; int cap; map_string* data; } list_map_string;
static list_map_string list_map_string_create(int cap){ list_map_string l; l.len=0; l.cap=cap; l.data=cap?(map_string*)malloc(sizeof(map_string)*cap):NULL; return l; }
static void list_map_string_push(list_map_string* l,map_string m){ if(l->len>=l->cap){ l->cap=l->cap?l->cap*2:4; l->data=(map_string*)realloc(l->data,sizeof(map_string)*l->cap);} l->data[l->len++]=m; }
static void _skip_ws(const char** s){ while(**s && (**s==' '||**s=='\t'||**s=='\n'||**s=='\r'))(*s)++; }
static char* _parse_string(const char** s){ const char* p=*s; if(*p!='"') return strdup(""); p++; const char* st=p; while(*p && *p!='"'){ if(*p=='\\'&&p[1]) p++; p++; } size_t len=p-st; char* out=(char*)malloc(len+1); memcpy(out,st,len); out[len]='\0'; if(*p=='"') p++; *s=p; return out; }
static int _isnum(char c){ return (c>='0'&&c<='9')||c=='-'||c=='+'||c=='.'; }
static char* _parse_token(const char** s){ const char* p=*s; const char* st=p; while(_isnum(*p)) p++; size_t len=p-st; char* out=(char*)malloc(len+1); memcpy(out,st,len); out[len]='\0'; *s=p; return out; }
static map_string _parse_json_obj(const char** s){ const char* p=*s; if(*p=='{') p++; map_string row=map_string_create(0); _skip_ws(&p); while(*p && *p!='}'){ _skip_ws(&p); char* k=_parse_string(&p); _skip_ws(&p); if(*p==':') p++; _skip_ws(&p); char* v; if(*p=='"') v=_parse_string(&p); else v=_parse_token(&p); map_string_put(&row,k,v); _skip_ws(&p); if(*p==','){ p++; _skip_ws(&p); } } if(*p=='}') p++; *s=p; return row; }
static list_map_string _parse_json(const char* text){ const char* p=text; _skip_ws(&p); list_map_string rows=list_map_string_create(0); if(*p=='['){ p++; _skip_ws(&p); while(*p && *p!=']'){ map_string r=_parse_json_obj(&p); list_map_string_push(&rows,r); _skip_ws(&p); if(*p==','){ p++; _skip_ws(&p); } } if(*p==']') p++; } else if(*p=='{'){ map_string r=_parse_json_obj(&p); list_map_string_push(&rows,r); } return rows; }
static list_map_string _load_json(const char* path){ char* text=_read_all(path); list_map_string rows=_parse_json(text); free(text); return rows; }
`
	helperSaveJSON = `static void _write_string(FILE* f,const char* s){ fputc('"',f); for(const char* p=s;*p;p++){ if(*p=='"'||*p=='\\') fputc('\\',f); fputc(*p,f);} fputc('"',f); }
static int _is_number(const char* s){ if(!*s) return 0; for(const char* p=s;*p;p++) if(!_isnum(*p)) return 0; return 1; }
static void _write_obj(FILE* f,map_string m){ fputc('{',f); for(int i=0;i<m.len;i++){ if(i>0) fputc(',',f); _write_string(f,m.data[i].key); fputc(':',f); if(_is_number(m.data[i].value)) fputs(m.data[i].value,f); else _write_string(f,m.data[i].value); } fputc('}',f); }
static void _save_json(list_map_string rows,const char* path){ FILE* f=(!path||path[0]=='\0'||strcmp(path,"-")==0)?stdout:fopen(path,"w"); if(!f){fprintf(stderr,"cannot open %s\n",path); exit(1);} if(rows.len==1){ _write_obj(f,rows.data[0]); } else { fputc('[',f); for(int i=0;i<rows.len;i++){ if(i>0) fputc(',',f); _write_obj(f,rows.data[i]); } fputc(']',f); } if(f!=stdout) fclose(f); }
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
	needListFloat:            helperListFloat,
	needListString:           helperListString,
	needListListInt:          helperListListInt,
	needConcatListInt:        helperConcatListInt,
	needConcatListFloat:      helperConcatListFloat,
	needConcatListString:     helperConcatListString,
	needConcatListListInt:    helperConcatListListInt,
	needConcatString:         helperConcatString,
	needUnionListInt:         helperUnionListInt,
	needUnionListListInt:     helperUnionListListInt,
	needUnionListFloat:       helperUnionListFloat,
	needUnionListString:      helperUnionListString,
	needExceptListInt:        helperExceptListInt,
	needExceptListListInt:    helperExceptListListInt,
	needExceptListFloat:      helperExceptListFloat,
	needExceptListString:     helperExceptListString,
	needIntersectListInt:     helperIntersectListInt,
	needIntersectListListInt: helperIntersectListListInt,
	needIntersectListFloat:   helperIntersectListFloat,
	needIntersectListString:  helperIntersectListString,
	needCount:                helperCount,
	needSumInt:               helperSumInt,
	needSumFloat:             helperSumFloat,
	needAvg:                  helperAvg,
	needAvgFloat:             helperAvgFloat,
	needInListInt:            helperContainsListInt,
	needInListFloat:          helperContainsListFloat,
	needInListString:         helperContainsListString,
	needInString:             helperContainsString,
	needInListListInt:        helperContainsListListInt,
	needInMapIntBool:         helperMapIntBool,
	needInput:                helperInput,
	needStr:                  helperStr,
	needNow:                  helperNow,
	needJSON:                 helperJSON,
	needLoadJSON:             helperLoadJSON,
	needSaveJSON:             helperSaveJSON,
	needMapIntBool:           helperMapIntBool,
	needIndexString:          helperIndexString,
	needSliceString:          helperSliceString,
	needSliceListInt:         helperSliceListInt,
	needSliceListFloat:       helperSliceListFloat,
	needSliceListString:      helperSliceListString,
	needPrintListInt:         helperPrintListInt,
	needPrintListFloat:       helperPrintListFloat,
	needPrintListString:      helperPrintListString,
	needPrintListListInt:     helperPrintListListInt,
	needGroupByInt:           helperGroupByInt,
	needListPairString:       helperListPairString,
	needGroupByPairString:    helperGroupByPairString,
}

var helperOrder = []string{
	needListFloat,
	needListString,
	needListListInt,
	needMapIntBool,
	needConcatListInt,
	needConcatListFloat,
	needConcatListString,
	needConcatListListInt,
	needConcatString,
	needUnionListInt,
	needUnionListListInt,
	needUnionListFloat,
	needUnionListString,
	needExceptListInt,
	needExceptListListInt,
	needExceptListFloat,
	needExceptListString,
	needIntersectListInt,
	needIntersectListListInt,
	needIntersectListFloat,
	needIntersectListString,
	needCount,
	needSumInt,
	needSumFloat,
	needAvg,
	needAvgFloat,
	needInListInt,
	needInListFloat,
	needInListString,
	needInString,
	needInListListInt,
	needInMapIntBool,
	needInput,
	needStr,
	needNow,
	needJSON,
	needLoadJSON,
	needSaveJSON,
	needIndexString,
	needSliceString,
	needSliceListInt,
	needSliceListFloat,
	needSliceListString,
	needPrintListInt,
	needPrintListFloat,
	needPrintListString,
	needPrintListListInt,
	needGroupByInt,
	needListPairString,
	needGroupByPairString,
}

func (c *Compiler) emitRuntime() {
	c.buf.WriteString(helperListInt)
	for _, h := range helperOrder {
		if c.has(h) {
			c.buf.WriteString(helperCode[h])
		}
	}
}
