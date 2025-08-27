(ns main (:refer-clojure :exclude [parseIntStr precedence parse needParen formatRec makeNice main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parseIntStr precedence parse needParen formatRec makeNice main)

(declare _read_file)

(def ^:dynamic formatRec_left nil)

(def ^:dynamic formatRec_res nil)

(def ^:dynamic formatRec_right nil)

(def ^:dynamic main__ nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_t nil)

(def ^:dynamic main_tStr nil)

(def ^:dynamic makeNice_root nil)

(def ^:dynamic needParen_c nil)

(def ^:dynamic needParen_p nil)

(def ^:dynamic parseIntStr_digits nil)

(def ^:dynamic parseIntStr_i nil)

(def ^:dynamic parseIntStr_n nil)

(def ^:dynamic parse_ch nil)

(def ^:dynamic parse_i nil)

(def ^:dynamic parse_left nil)

(def ^:dynamic parse_op nil)

(def ^:dynamic parse_ops nil)

(def ^:dynamic parse_right nil)

(def ^:dynamic parse_vals nil)

(defn parseIntStr [parseIntStr_str]
  (binding [parseIntStr_digits nil parseIntStr_i nil parseIntStr_n nil] (try (do (set! parseIntStr_digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (set! parseIntStr_i 0) (set! parseIntStr_n 0) (while (< parseIntStr_i (count parseIntStr_str)) (do (set! parseIntStr_n (+' (*' parseIntStr_n 10) (Long/parseLong (get parseIntStr_digits (subs parseIntStr_str parseIntStr_i (min (+' parseIntStr_i 1) (count parseIntStr_str))))))) (set! parseIntStr_i (+' parseIntStr_i 1)))) (throw (ex-info "return" {:v parseIntStr_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn precedence [precedence_op]
  (try (do (when (or (= precedence_op "+") (= precedence_op "-")) (throw (ex-info "return" {:v 1}))) (if (or (= precedence_op "*") (= precedence_op "/")) 2 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parse [parse_s]
  (binding [parse_ch nil parse_i nil parse_left nil parse_op nil parse_ops nil parse_right nil parse_vals nil] (try (do (set! parse_ops []) (set! parse_vals []) (set! parse_i 0) (while (< parse_i (count parse_s)) (do (set! parse_ch (subs parse_s parse_i (min (+' parse_i 1) (count parse_s)))) (if (and (>= (compare parse_ch "a") 0) (<= (compare parse_ch "z") 0)) (set! parse_vals (conj parse_vals {:kind "var" :val parse_ch})) (if (= parse_ch "(") (set! parse_ops (conj parse_ops parse_ch)) (if (= parse_ch ")") (do (while (and (> (count parse_ops) 0) (not= (nth parse_ops (- (count parse_ops) 1)) "(")) (do (set! parse_op (nth parse_ops (- (count parse_ops) 1))) (set! parse_ops (subvec parse_ops 0 (- (count parse_ops) 1))) (set! parse_right (nth parse_vals (- (count parse_vals) 1))) (set! parse_vals (subvec parse_vals 0 (- (count parse_vals) 1))) (set! parse_left (nth parse_vals (- (count parse_vals) 1))) (set! parse_vals (subvec parse_vals 0 (- (count parse_vals) 1))) (set! parse_vals (conj parse_vals {:kind "op" :parse_left parse_left :parse_op parse_op :parse_right parse_right})))) (set! parse_ops (subvec parse_ops 0 (- (count parse_ops) 1)))) (do (while (and (and (> (count parse_ops) 0) (not= (nth parse_ops (- (count parse_ops) 1)) "(")) (>= (precedence (nth parse_ops (- (count parse_ops) 1))) (precedence parse_ch))) (do (set! parse_op (nth parse_ops (- (count parse_ops) 1))) (set! parse_ops (subvec parse_ops 0 (- (count parse_ops) 1))) (set! parse_right (nth parse_vals (- (count parse_vals) 1))) (set! parse_vals (subvec parse_vals 0 (- (count parse_vals) 1))) (set! parse_left (nth parse_vals (- (count parse_vals) 1))) (set! parse_vals (subvec parse_vals 0 (- (count parse_vals) 1))) (set! parse_vals (conj parse_vals {:kind "op" :parse_left parse_left :parse_op parse_op :parse_right parse_right})))) (set! parse_ops (conj parse_ops parse_ch)))))) (set! parse_i (+' parse_i 1)))) (while (> (count parse_ops) 0) (do (set! parse_op (nth parse_ops (- (count parse_ops) 1))) (set! parse_ops (subvec parse_ops 0 (- (count parse_ops) 1))) (set! parse_right (nth parse_vals (- (count parse_vals) 1))) (set! parse_vals (subvec parse_vals 0 (- (count parse_vals) 1))) (set! parse_left (nth parse_vals (- (count parse_vals) 1))) (set! parse_vals (subvec parse_vals 0 (- (count parse_vals) 1))) (set! parse_vals (conj parse_vals {:kind "op" :parse_left parse_left :parse_op parse_op :parse_right parse_right})))) (throw (ex-info "return" {:v (nth parse_vals (- (count parse_vals) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn needParen [needParen_parent needParen_isRight needParen_child]
  (binding [needParen_c nil needParen_p nil] (try (do (when (not= (:kind needParen_child) "op") (throw (ex-info "return" {:v false}))) (set! needParen_p (precedence needParen_parent)) (set! needParen_c (precedence (:op needParen_child))) (when (< needParen_c needParen_p) (throw (ex-info "return" {:v true}))) (when (> needParen_c needParen_p) (throw (ex-info "return" {:v false}))) (when (and (and (= needParen_parent "-") needParen_isRight) (or (= (:op needParen_child) "+") (= (:op needParen_child) "-"))) (throw (ex-info "return" {:v true}))) (if (and (and (= needParen_parent "/") needParen_isRight) (or (= (:op needParen_child) "*") (= (:op needParen_child) "/"))) true false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn formatRec [formatRec_node formatRec_parent formatRec_isRight]
  (binding [formatRec_left nil formatRec_res nil formatRec_right nil] (try (do (when (not= (:kind formatRec_node) "op") (throw (ex-info "return" {:v (:val formatRec_node)}))) (set! formatRec_left (formatRec (:left formatRec_node) (:op formatRec_node) false)) (set! formatRec_right (formatRec (:right formatRec_node) (:op formatRec_node) true)) (set! formatRec_res (str (str formatRec_left (:op formatRec_node)) formatRec_right)) (when (and (not= formatRec_parent "") (needParen formatRec_parent formatRec_isRight formatRec_node)) (set! formatRec_res (str (str "(" formatRec_res) ")"))) (throw (ex-info "return" {:v formatRec_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn makeNice [makeNice_s]
  (binding [makeNice_root nil] (try (do (set! makeNice_root (parse makeNice_s)) (throw (ex-info "return" {:v (formatRec makeNice_root "" false)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main__ nil main_line nil main_t nil main_tStr nil] (try (do (set! main_tStr (read-line)) (when (= main_tStr "") (throw (ex-info "return" {:v nil}))) (set! main_t (parseIntStr main_tStr)) (dotimes [main__ main_t] (do (set! main_line (read-line)) (println (makeNice main_line))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
