(ns main (:refer-clojure :exclude [suffix_tree_new suffix_tree_search]))

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

(declare suffix_tree_new suffix_tree_search)

(def ^:dynamic main_i nil)

(def ^:dynamic suffix_tree_search_found nil)

(def ^:dynamic suffix_tree_search_i nil)

(def ^:dynamic suffix_tree_search_j nil)

(def ^:dynamic suffix_tree_search_m nil)

(def ^:dynamic suffix_tree_search_n nil)

(defn suffix_tree_new [suffix_tree_new_text]
  (try (throw (ex-info "return" {:v {:text suffix_tree_new_text}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn suffix_tree_search [suffix_tree_search_st suffix_tree_search_pattern]
  (binding [suffix_tree_search_found nil suffix_tree_search_i nil suffix_tree_search_j nil suffix_tree_search_m nil suffix_tree_search_n nil] (try (do (when (= (count suffix_tree_search_pattern) 0) (throw (ex-info "return" {:v true}))) (set! suffix_tree_search_i 0) (set! suffix_tree_search_n (count (:text suffix_tree_search_st))) (set! suffix_tree_search_m (count suffix_tree_search_pattern)) (while (<= suffix_tree_search_i (- suffix_tree_search_n suffix_tree_search_m)) (do (set! suffix_tree_search_j 0) (set! suffix_tree_search_found true) (loop [while_flag_1 true] (when (and while_flag_1 (< suffix_tree_search_j suffix_tree_search_m)) (cond (not= (get (:text suffix_tree_search_st) (+ suffix_tree_search_i suffix_tree_search_j)) (nth suffix_tree_search_pattern suffix_tree_search_j)) (do (set! suffix_tree_search_found false) (recur false)) :else (do (set! suffix_tree_search_j (+ suffix_tree_search_j 1)) (recur while_flag_1))))) (when suffix_tree_search_found (throw (ex-info "return" {:v true}))) (set! suffix_tree_search_i (+ suffix_tree_search_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_text "banana")

(def ^:dynamic main_st (suffix_tree_new main_text))

(def ^:dynamic main_patterns_exist ["ana" "ban" "na"])

(def ^:dynamic main_i 0)

(def ^:dynamic main_patterns_none ["xyz" "apple" "cat"])

(def ^:dynamic main_substrings ["ban" "ana" "a" "na"])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i (count main_patterns_exist)) (do (println (str (suffix_tree_search main_st (nth main_patterns_exist main_i)))) (def main_i (+ main_i 1))))
      (def main_i 0)
      (while (< main_i (count main_patterns_none)) (do (println (str (suffix_tree_search main_st (nth main_patterns_none main_i)))) (def main_i (+ main_i 1))))
      (println (str (suffix_tree_search main_st "")))
      (println (str (suffix_tree_search main_st main_text)))
      (def main_i 0)
      (while (< main_i (count main_substrings)) (do (println (str (suffix_tree_search main_st (nth main_substrings main_i)))) (def main_i (+ main_i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
