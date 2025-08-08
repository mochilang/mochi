(ns main (:refer-clojure :exclude [pop_last balanced_parentheses]))

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

(declare pop_last balanced_parentheses)

(def ^:dynamic balanced_parentheses_ch nil)

(def ^:dynamic balanced_parentheses_i nil)

(def ^:dynamic balanced_parentheses_pairs nil)

(def ^:dynamic balanced_parentheses_stack nil)

(def ^:dynamic balanced_parentheses_top nil)

(def ^:dynamic main_idx nil)

(def ^:dynamic pop_last_i nil)

(def ^:dynamic pop_last_res nil)

(defn pop_last [pop_last_xs]
  (binding [pop_last_i nil pop_last_res nil] (try (do (set! pop_last_res []) (set! pop_last_i 0) (while (< pop_last_i (- (count pop_last_xs) 1)) (do (set! pop_last_res (conj pop_last_res (nth pop_last_xs pop_last_i))) (set! pop_last_i (+ pop_last_i 1)))) (throw (ex-info "return" {:v pop_last_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn balanced_parentheses [balanced_parentheses_s]
  (binding [balanced_parentheses_ch nil balanced_parentheses_i nil balanced_parentheses_pairs nil balanced_parentheses_stack nil balanced_parentheses_top nil] (try (do (set! balanced_parentheses_stack []) (set! balanced_parentheses_pairs {"(" ")" "[" "]" "{" "}"}) (set! balanced_parentheses_i 0) (while (< balanced_parentheses_i (count balanced_parentheses_s)) (do (set! balanced_parentheses_ch (nth balanced_parentheses_s balanced_parentheses_i)) (if (in balanced_parentheses_ch balanced_parentheses_pairs) (set! balanced_parentheses_stack (conj balanced_parentheses_stack balanced_parentheses_ch)) (when (or (or (= balanced_parentheses_ch ")") (= balanced_parentheses_ch "]")) (= balanced_parentheses_ch "}")) (do (when (= (count balanced_parentheses_stack) 0) (throw (ex-info "return" {:v false}))) (set! balanced_parentheses_top (nth balanced_parentheses_stack (- (count balanced_parentheses_stack) 1))) (when (not= (nth balanced_parentheses_pairs balanced_parentheses_top) balanced_parentheses_ch) (throw (ex-info "return" {:v false}))) (set! balanced_parentheses_stack (pop_last balanced_parentheses_stack))))) (set! balanced_parentheses_i (+ balanced_parentheses_i 1)))) (throw (ex-info "return" {:v (= (count balanced_parentheses_stack) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_tests ["([]{})" "[()]{}{[()()]()}" "[(])" "1+2*3-4" ""])

(def ^:dynamic main_idx 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_idx (count main_tests)) (do (println (balanced_parentheses (nth main_tests main_idx))) (def main_idx (+ main_idx 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
