(ns main (:refer-clojure :exclude [new_suffix_tree search main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare new_suffix_tree search main)

(def ^:dynamic main_found nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_pattern nil)

(def ^:dynamic main_patterns nil)

(def ^:dynamic main_suffix_tree nil)

(def ^:dynamic main_text nil)

(def ^:dynamic search_i nil)

(def ^:dynamic search_m nil)

(def ^:dynamic search_n nil)

(defn new_suffix_tree [new_suffix_tree_text]
  (try (throw (ex-info "return" {:v {:text new_suffix_tree_text}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn search [search_tree search_pattern]
  (binding [search_i nil search_m nil search_n nil] (try (do (set! search_n (count (:text search_tree))) (set! search_m (count search_pattern)) (when (= search_m 0) (throw (ex-info "return" {:v true}))) (when (> search_m search_n) (throw (ex-info "return" {:v false}))) (set! search_i 0) (while (<= search_i (- search_n search_m)) (do (when (= (subvec (:text search_tree) search_i (+ search_i search_m)) search_pattern) (throw (ex-info "return" {:v true}))) (set! search_i (+ search_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_found nil main_i nil main_pattern nil main_patterns nil main_suffix_tree nil main_text nil] (do (set! main_text "monkey banana") (set! main_suffix_tree (new_suffix_tree main_text)) (set! main_patterns ["ana" "ban" "na" "xyz" "mon"]) (set! main_i 0) (while (< main_i (count main_patterns)) (do (set! main_pattern (nth main_patterns main_i)) (set! main_found (search main_suffix_tree main_pattern)) (println (str (str (str "Pattern '" main_pattern) "' found: ") (str main_found))) (set! main_i (+ main_i 1)))))))

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
