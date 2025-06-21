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
`

	helperGroup = `(defrecord _Group [key Items])
`

	helperGroupBy = `(defn _group_by [src keyfn]
  (let [groups (atom {})
        order (atom [])]
    (doseq [it src]
      (let [k (keyfn it)
            ks (str k)]
        (when-not (contains? @groups ks)
          (swap! groups assoc ks (_Group. k []))
          (swap! order conj ks))
        (swap! groups update ks (fn [g] (assoc g :Items (conj (:Items g) it)))))
    )
    (map (fn [k] (@groups k)) @order)))
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
        (let [data (clojure.data.json/read-str text :key-fn keyword)]
          (cond
            (map? data) [data]
            (sequential? data) (vec data)
            :else []))
      (= fmt "jsonl")
        (->> (clojure.string/split-lines text)
             (remove clojure.string/blank?)
             (mapv #(clojure.data.json/read-str % :key-fn keyword)))
      (= fmt "yaml")
        (let [y (-> text java.io.StringReader. (org.yaml.snakeyaml.Yaml.) .load)]
          (cond
            (instance? java.util.Map y) [(into {} y)]
            (instance? java.util.List y) (mapv #(into {} %) y)
            :else []))
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
        (let [out (clojure.data.json/write-str rows)]
          (if (or (nil? path) (= path "") (= path "-"))
            (print out)
            (spit path out)))
      (= fmt "jsonl")
        (let [out (clojure.string/join "\n" (map #(clojure.data.json/write-str %) rows))]
          (if (or (nil? path) (= path "") (= path "-"))
            (print (str out "\n"))
            (spit path (str out "\n"))))
      (= fmt "yaml")
        (let [yaml (org.yaml.snakeyaml.Yaml.)
              out (.dump yaml (clojure.walk/keywordize-keys
                             (if (= 1 (count rows)) (first rows) rows)))]
          (if (or (nil? path) (= path "") (= path "-"))
            (print out)
            (spit path out)))
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
        fields (first (:arglists (meta ctor)))
        args (map #(get m %) fields)]
    (apply ctor args)))`

	helperFetch = `(defn _fetch [url opts]
  (let [txt (slurp url)]
    (clojure.data.json/read-str txt :key-fn keyword))`
)

var helperMap = map[string]string{
	"_indexString": helperIndexString,
	"_indexList":   helperIndexList,
	"_input":       helperInput,
	"_count":       helperCount,
	"_avg":         helperAvg,
	"_Group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_parse_csv":   helperParseCSV,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_escape_json": helperEscapeJSON,
	"_to_json":     helperToJSON,
	"_json":        helperJSON,
	"_in":          helperIn,
	"_union_all":   helperUnionAll,
	"_union":       helperUnion,
	"_except":      helperExcept,
	"_intersect":   helperIntersect,
	"_gen_text":    helperGenText,
	"_gen_embed":   helperGenEmbed,
	"_gen_struct":  helperGenStruct,
	"_fetch":       helperFetch,
}

var helperOrder = []string{
	"_indexString",
	"_indexList",
	"_input",
	"_count",
	"_avg",
	"_Group",
	"_group_by",
	"_parse_csv",
	"_load",
	"_save",
	"_escape_json",
	"_to_json",
	"_json",
	"_in",
	"_union_all",
	"_union",
	"_except",
	"_intersect",
	"_gen_text",
	"_gen_embed",
	"_gen_struct",
	"_fetch",
}

// helperDeps lists transitive helper dependencies.
// When a helper is used, its dependencies are also emitted.
var helperDeps = map[string][]string{
	"_json":    {"_to_json"},
	"_to_json": {"_escape_json"},
	"_load":    {"_parse_csv"},
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
