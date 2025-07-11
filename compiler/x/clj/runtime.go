//go:build slow

package cljcode

import (
	"bytes"
)

// Runtime helper functions for the Clojure backend.
const (
	helperIndexString = `(defn _indexString [s i]
  (let [r (vec (seq s))
        i (if (neg? i) (+ i (count r)) i)]
    (if (or (< i 0) (>= i (count r)))
      (throw (ex-info "index out of range" {}))
      (str (nth r i)))))
`

	helperIndexList = `(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))
`

	helperInput = `(defn _input []
  (clojure.string/trim (read-line)))
`

	helperCount = `(defn _count [v]
  (cond
    (sequential? v) (count v)
    (and (map? v) (contains? v :Items)) (count (:Items v))
    :else (throw (ex-info "count() expects list or group" {}))))
`

	helperAvg = `(defn _avg [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "avg() expects list or group" {})))]
    (if (empty? lst)
      0
      (/ (reduce + lst) (double (count lst)))))
  )
`

	helperSum = `(defn _sum [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "sum() expects list or group" {})))]
    (reduce + 0 lst))
  )
`

	helperMin = `(defn _min [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "min() expects list or group" {})))]
    (if (empty? lst)
      0
      (reduce (fn [a b] (if (neg? (compare a b)) a b)) lst))))
`

	helperMax = `(defn _max [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "max() expects list or group" {})))]
    (if (empty? lst)
      0
      (reduce (fn [a b] (if (pos? (compare a b)) a b)) lst)))
  )
`

	helperGroup = `(defrecord _Group [key Items])
`

	helperGroupBy = `(defn _group_by [src keyfn]
  (let [groups (transient {})
        order (transient [])]
    (doseq [it src]
      (let [k (keyfn it)
            ks (str k)
            g (get groups ks)]
        (if g
          (assoc! groups ks (assoc g :Items (conj (:Items g) it)))
          (do
            (assoc! groups ks (_Group. k [it]))
            (conj! order ks))))
    (let [g (persistent! groups)
          o (persistent! order)]
      (mapv #(get g %) o))) )
`

	helperParseCSV = `(defn _parse_csv [text header delim]
  (let [lines (->> (clojure.string/split-lines text)
                   (remove clojure.string/blank?))
        headers (if header
                    (clojure.string/split (first lines) (re-pattern (str delim)))
                    (map #(str "c" %) (range (count (clojure.string/split (first lines) (re-pattern (str delim)))))))]
    (mapv (fn [line]
            (let [parts (clojure.string/split line (re-pattern (str delim)))]
              (zipmap headers parts)))
          (drop (if header 1 0) lines))) )
`

	helperLoad = `(defn _load [path opts]
  (let [fmt (get opts :format "csv")
        header (get opts :header true)
        delim (first (or (get opts :delimiter ",") ","))
        text (if (or (nil? path) (= path "") (= path "-"))
               (slurp *in*)
               (slurp path))]
    (cond
      (= fmt "csv") (_parse_csv text header delim)
      (= fmt "tsv") (_parse_csv text header "\t")
      (= fmt "json")
        (let [data (clojure.edn/read-string text)]
          (cond
            (map? data) [data]
            (sequential? data) (vec data)
            :else []))
      (= fmt "jsonl")
        (->> (clojure.string/split-lines text)
             (remove clojure.string/blank?)
             (mapv clojure.edn/read-string))
      (= fmt "yaml")
        (->> (clojure.string/split text #"(?m)^- ")
             (map clojure.string/trim)
             (remove clojure.string/blank?)
             (mapv (fn [blk]
                     (->> (clojure.string/split-lines blk)
                          (map clojure.string/trim)
                          (remove clojure.string/blank?)
                          (map (fn [line]
                                 (let [[k v] (clojure.string/split line #":\s*" 2)]
                                   [(keyword k) v])))
                          (into {})))))
      :else [])) )
`

	helperSave = `(defn _save [rows path opts]
  (let [fmt (get opts :format "csv")
        header (get opts :header false)
        delim (first (or (get opts :delimiter ",") ","))
        headers (if (seq rows) (sort (keys (first rows))) [])]
    (cond
      (= fmt "csv")
        (let [lines (concat
                      (when header [(clojure.string/join delim headers)])
                      (map (fn [r]
                             (clojure.string/join delim (map #(str (get r % "")) headers)))
                           rows))
              out (str (clojure.string/join "\n" lines) "\n")]
          (if (or (nil? path) (= path "") (= path "-"))
            (print out)
            (spit path out)))
      (= fmt "tsv")
        (_save rows path (assoc opts :format "csv" :delimiter "\t"))
      (= fmt "json")
        (let [out (_to_json rows)]
          (if (or (nil? path) (= path "") (= path "-"))
            (print out)
            (spit path out)))
      (= fmt "jsonl")
        (let [out (clojure.string/join "\n" (map _to_json rows))]
          (if (or (nil? path) (= path "") (= path "-"))
            (print (str out "\n"))
            (spit path (str out "\n"))))
      :else
        nil)) )
`

	helperEscapeJSON = `(defn _escape_json [s]
  (-> s
      (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")))
`

	helperToJSON = `(defn _to_json [v]
  (cond
    (nil? v) "null"
    (string? v) (str "\"" (_escape_json v) "\"")
    (number? v) (str v)
    (boolean? v) (str v)
    (sequential? v) (str "[" (clojure.string/join "," (map _to_json v)) "]")
    (map? v) (str "{" (clojure.string/join "," (map (fn [[k val]]
                                        (str "\"" (_escape_json (name k)) "\":" (_to_json val))) v)) "}")
    :else (str "\"" (_escape_json (str v)) "\"")))
`

	helperJSON = `(defn _json [v]
  (println (_to_json v)))
`

	helperSortKey = `(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))`

	helperIn = `(defn _in [item col]
  (cond
    (and (string? col) (string? item)) (clojure.string/includes? col item)
    (map? col) (contains? col item)
    (sequential? col) (some #(= item %) col)
    :else false))`

	helperUnionAll = `(defn _union_all [a b]
  (vec (concat a b)))`

	helperUnion = `(defn _union [a b]
  (vec (distinct (concat a b))))`

	helperExcept = `(defn _except [a b]
  (vec (remove (set b) a)))`

	helperIntersect = `(defn _intersect [a b]
  (vec (distinct (filter (set b) a))))`

	helperGenText = `(defn _gen_text [prompt model params]
  prompt)`

	helperGenEmbed = `(defn _gen_embed [text model params]
  (mapv double (map int text)))`

	helperGenStruct = `(defn _gen_struct [ctor prompt model params]
  (let [m (clojure.data.json/read-str prompt :key-fn keyword)
        fields (some->> ctor meta :arglists first (map keyword))
        args (map #(get m %) fields)]
    (apply ctor args)))`

	helperCastStruct = `(defn _cast_struct [ctor m]
  (let [fields (or (some->> ctor meta :arglists first (map keyword))
                   (keys m))]
    (apply ctor (map #(get m %) fields))))`

	helperCastStructList = `(defn _cast_struct_list [ctor xs]
  (mapv #(_cast_struct ctor %) xs))`

	helperFetch = `(defn _fetch [url opts]
  ;; Ensure IPv4 is preferred to avoid network issues in some environments
  (System/setProperty "java.net.preferIPv4Stack" "true")
  (let [method (get opts :method "GET")
        q      (get opts :query nil)
        url     (if q
                   (let [qs (clojure.string/join "&"
                             (map (fn [[k v]]
                                    (str (java.net.URLEncoder/encode (name k) "UTF-8")
                                         "="
                                         (java.net.URLEncoder/encode (str v) "UTF-8")))
                                  q))
                         sep (if (clojure.string/includes? url "?") "&" "?")]
                     (str url sep qs))
                   url)]
    (cond
      (or (clojure.string/starts-with? url "file://")
          (clojure.string/starts-with? url "file:"))
        (let [path (if (clojure.string/starts-with? url "file://")
                     (subs url 7)
                     (subs url 5))
              txt  (try
                     (slurp path)
                     (catch java.io.FileNotFoundException _
                       (let [alt (str "../../.." "/" path)]
                         (slurp alt))))]
          (clojure.data.json/read-str txt :key-fn keyword))
      :else
        (let [builder (doto (java.net.http.HttpRequest/newBuilder (java.net.URI/create url))
                        (.method method
                                (if (contains? opts :body)
                                  (java.net.http.HttpRequest$BodyPublishers/ofString
                                    (clojure.data.json/write-str (:body opts)))
                                  (java.net.http.HttpRequest$BodyPublishers/noBody))))]
          (when-let [hs (:headers opts)]
            (doseq [[k v] hs]
              (.header builder (name k) (str v))))
          (when-let [t (:timeout opts)]
            (.timeout builder (java.time.Duration/ofSeconds (long t))))
          (let [client (java.net.http.HttpClient/newHttpClient)
                resp (.send client (.build builder)
                            (java.net.http.HttpResponse$BodyHandlers/ofString))]
            (clojure.data.json/read-str (.body resp) :key-fn keyword)))))
  )`

	helperQuery = `(defn _query [src joins opts]
  (let [items (atom (mapv vector src))]
    (doseq [j joins]
      (let [joined (atom [])]
        (if (and (:leftKey j) (:rightKey j))
          (let [idx (group-by (:rightKey j) (:items j))
                seen (atom #{})]
            (doseq [left @items]
              (let [k ((:leftKey j) left)
                    rs (get idx k)]
                (if rs
                  (do
                    (swap! seen conj k)
                    (doseq [right rs]
                      (swap! joined conj (conj left right))))
                  (when (:left j)
                    (swap! joined conj (conj left nil)))))
            (when (:right j)
              (doseq [right (:items j)]
                (when-not (contains? @seen ((:rightKey j) right))
                  (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right]))))))
            (reset! items @joined))
          (cond
            (and (:right j) (:left j))
              (let [matched (boolean-array (count (:items j)))]
                (doseq [left @items]
                  (let [m (atom false)]
                    (doseq [[ri right] (map-indexed vector (:items j))]
                      (let [keep (if-let [f (:on j)]
                                   (apply f (conj left right))
                                   true)]
                        (when keep
                          (reset! m true)
                          (aset matched ri true)
                          (swap! joined conj (conj left right))))
                    (when-not @m
                      (swap! joined conj (conj left nil))))
                (doseq [[ri right] (map-indexed vector (:items j))]
                  (when-not (aget matched ri)
                    (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
              (reset! items @joined)
            (:right j)
              (do
                (doseq [right (:items j)]
                  (let [m (atom false)]
                    (doseq [left @items]
                      (let [keep (if-let [f (:on j)]
                                   (apply f (conj left right))
                                   true)]
                        (when keep
                          (reset! m true)
                          (swap! joined conj (conj left right))))
                    (when-not @m
                      (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
                (reset! items @joined))
            :else
              (do
                (doseq [left @items]
                  (let [m (atom false)]
                    (doseq [right (:items j)]
                      (let [keep (if-let [f (:on j)]
                                   (apply f (conj left right))
                                   true)]
                        (when keep
                          (reset! m true)
                          (swap! joined conj (conj left right))))
                    (when (and (:left j) (not @m))
                      (swap! joined conj (conj left nil))))
                (reset! items @joined)))))
    (let [it @items
          it (if-let [w (:where opts)] (vec (filter #(apply w %) it)) it)
          it (if-let [sk (:sortKey opts)]
               (vec (sort-by #(let [k (apply sk %)] (_sort_key k)) it))
               it)
          it (if (contains? opts :skip) (vec (drop (:skip opts) it)) it)
          it (if (contains? opts :take) (vec (take (:take opts) it)) it)]
      (mapv #(apply (:select opts) %) it))))))))))))`
)

var helperMap = map[string]string{
	"_indexString":      helperIndexString,
	"_indexList":        helperIndexList,
	"_input":            helperInput,
	"_count":            helperCount,
	"_avg":              helperAvg,
	"_sum":              helperSum,
	"_min":              helperMin,
	"_max":              helperMax,
	"_Group":            helperGroup,
	"_group_by":         helperGroupBy,
	"_parse_csv":        helperParseCSV,
	"_load":             helperLoad,
	"_save":             helperSave,
	"_escape_json":      helperEscapeJSON,
	"_to_json":          helperToJSON,
	"_json":             helperJSON,
	"_sort_key":         helperSortKey,
	"_in":               helperIn,
	"_union_all":        helperUnionAll,
	"_union":            helperUnion,
	"_except":           helperExcept,
	"_intersect":        helperIntersect,
	"_gen_text":         helperGenText,
	"_gen_embed":        helperGenEmbed,
	"_gen_struct":       helperGenStruct,
	"_cast_struct":      helperCastStruct,
	"_cast_struct_list": helperCastStructList,
	"_fetch":            helperFetch,
	"_query":            helperQuery,
}

var helperOrder = []string{
	"_indexString",
	"_indexList",
	"_input",
	"_count",
	"_avg",
	"_sum",
	"_min",
	"_max",
	"_Group",
	"_group_by",
        "_parse_csv",
        "_load",
        "_escape_json",
        "_to_json",
        "_save",
	"_json",
	"_sort_key",
	"_in",
	"_union_all",
	"_union",
	"_except",
	"_intersect",
	"_gen_text",
	"_gen_embed",
	"_gen_struct",
	"_cast_struct",
	"_cast_struct_list",
	"_fetch",
	"_query",
}

// helperDeps lists transitive helper dependencies.
// When a helper is used, its dependencies are also emitted.
var helperDeps = map[string][]string{
        "_json":             {"_to_json"},
        "_to_json":          {"_escape_json"},
        "_load":             {"_parse_csv"},
        "_save":             {"_to_json"},
        "_cast_struct_list": {"_cast_struct"},
}

func (c *Compiler) use(name string) {
	if c.helpers == nil {
		return
	}
	if c.helpers[name] {
		return
	}
	c.helpers[name] = true
	if deps, ok := helperDeps[name]; ok {
		for _, d := range deps {
			c.use(d)
		}
	}
}

func (c *Compiler) emitRuntime(buf *bytes.Buffer) {
	if len(c.helpers) == 0 {
		return
	}
	for _, name := range helperOrder {
		if c.helpers[name] {
			buf.WriteString(helperMap[name])
			buf.WriteByte('\n')
		}
	}
}
