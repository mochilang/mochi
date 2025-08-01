(ns main (:refer-clojure :exclude [join sortPairs isAlphaNumDot main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare join sortPairs isAlphaNumDot main)

(declare arr before cc ch freq i idx j k order p pairs res skip src srcLines tmp token)

(defn join [xs sep]
  (try (do (def res "") (def i 0) (while (< i (count xs)) (do (when (> i 0) (def res (str res sep))) (def res (str res (nth xs i))) (def i (+' i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sortPairs [xs]
  (try (do (def arr xs) (def i 1) (while (< i (count arr)) (do (def j i) (while (and (> j 0) (< (int (get (nth arr (- j 1)) "count")) (int (get (nth arr j) "count")))) (do (def tmp (nth arr (- j 1))) (def arr (assoc arr (- j 1) (nth arr j))) (def arr (assoc arr j tmp)) (def j (- j 1)))) (def i (+' i 1)))) (throw (ex-info "return" {:v arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isAlphaNumDot [ch]
  (try (throw (ex-info "return" {:v (or (or (or (or (and (>= (compare ch "A") 0) (<= (compare ch "Z") 0)) (and (>= (compare ch "a") 0) (<= (compare ch "z") 0))) (and (>= (compare ch "0") 0) (<= (compare ch "9") 0))) (= ch "_")) (= ch "."))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def srcLines ["package main" "" "import (" "    \"fmt\"" "    \"go/ast\"" "    \"go/parser\"" "    \"go/token\"" "    \"io/ioutil\"" "    \"os\"" "    \"sort\"" ")" "" "func main() {" "    if len(os.Args) != 2 {" "        fmt.Println(\"usage ff <go source filename>\")" "        return" "    }" "    src, err := ioutil.ReadFile(os.Args[1])" "    if err != nil {" "        fmt.Println(err)" "        return" "    }" "    fs := token.NewFileSet()" "    a, err := parser.ParseFile(fs, os.Args[1], src, 0)" "    if err != nil {" "        fmt.Println(err)" "        return" "    }" "    f := fs.File(a.Pos())" "    m := make(map[string]int)" "    ast.Inspect(a, func(n ast.Node) bool {" "        if ce, ok := n.(*ast.CallExpr); ok {" "            start := f.Offset(ce.Pos())" "            end := f.Offset(ce.Lparen)" "            m[string(src[start:end])]++" "        }" "        return true" "    })" "    cs := make(calls, 0, len(m))" "    for k, v := range m {" "        cs = append(cs, &call{k, v})" "    }" "    sort.Sort(cs)" "    for i, c := range cs {" "        fmt.Printf(\"%-20s %4d\\n\", c.expr, c.count)" "        if i == 9 {" "            break" "        }" "    }" "}" "" "type call struct {" "    expr  string" "    count int" "}" "type calls []*call" "" "func (c calls) Len() int           { return len(c) }" "func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }" "func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }"]) (def src (join srcLines "\n")) (def freq {}) (def i 0) (def order []) (while (< i (count src)) (do (def ch (subs src i (+' i 1))) (if (or (or (and (>= (compare ch "A") 0) (<= (compare ch "Z") 0)) (and (>= (compare ch "a") 0) (<= (compare ch "z") 0))) (= ch "_")) (do (def j (+' i 1)) (while (and (< j (count src)) (isAlphaNumDot (subs src j (+' j 1)))) (def j (+' j 1))) (def token (subs src i j)) (def k j) (loop [while_flag_1 true] (when (and while_flag_1 (< k (count src))) (do (def cc (subs src k (+' k 1))) (if (or (or (or (= cc " ") (= cc "\t")) (= cc "\n")) (= cc "\r")) (def k (+' k 1)) (recur false)) (cond :else (recur while_flag_1))))) (when (and (< k (count src)) (= (subs src k (+' k 1)) "(")) (do (def p (- i 1)) (while (and (>= p 0) (or (= (subs src p (+' p 1)) " ") (= (subs src p (+' p 1)) "\t"))) (def p (- p 1))) (def skip false) (when (>= p 3) (do (def before (subs src (- p 3) (+' p 1))) (when (= before "func") (def skip true)))) (when (not skip) (if (in token freq) (def freq (assoc freq token (+' (get freq token) 1))) (do (def freq (assoc freq token 1)) (def order (conj order token))))))) (def i j)) (def i (+' i 1))))) (def pairs []) (doseq [t order] (def pairs (conj pairs {"expr" t "count" (nth freq t)}))) (def pairs (sortPairs pairs)) (def idx 0) (while (and (< idx (count pairs)) (< idx 10)) (do (def p (nth pairs idx)) (println (str (str (get p "expr") " ") (str (get p "count")))) (def idx (+' idx 1))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
