(ns main (:refer-clojure :exclude [autocomplete_using_trie]))

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
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare autocomplete_using_trie)

(def ^:dynamic autocomplete_using_trie_i nil)

(def ^:dynamic autocomplete_using_trie_result nil)

(def ^:dynamic autocomplete_using_trie_w nil)

(def ^:dynamic main_words ["depart" "detergent" "daring" "dog" "deer" "deal"])

(defn autocomplete_using_trie [autocomplete_using_trie_prefix]
  (binding [autocomplete_using_trie_i nil autocomplete_using_trie_result nil autocomplete_using_trie_w nil] (try (do (set! autocomplete_using_trie_result []) (set! autocomplete_using_trie_i 0) (while (< autocomplete_using_trie_i (count main_words)) (do (set! autocomplete_using_trie_w (nth main_words autocomplete_using_trie_i)) (when (= (subvec autocomplete_using_trie_w 0 (count autocomplete_using_trie_prefix)) autocomplete_using_trie_prefix) (set! autocomplete_using_trie_result (conj autocomplete_using_trie_result (str autocomplete_using_trie_w " ")))) (set! autocomplete_using_trie_i (+ autocomplete_using_trie_i 1)))) (throw (ex-info "return" {:v autocomplete_using_trie_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (autocomplete_using_trie "de")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
