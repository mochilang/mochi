(ns main (:refer-clojure :exclude [indexOfStr stringSearchSingle stringSearch display main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOfStr stringSearchSingle stringSearch display main)

(declare display_i display_s indexOfStr_hlen indexOfStr_i indexOfStr_nlen main_i main_idxs main_j main_patterns main_texts stringSearch_hlen stringSearch_idx stringSearch_nlen stringSearch_result stringSearch_start)

(defn indexOfStr [indexOfStr_h indexOfStr_n]
  (try (do (def indexOfStr_hlen (count indexOfStr_h)) (def indexOfStr_nlen (count indexOfStr_n)) (when (= indexOfStr_nlen 0) (throw (ex-info "return" {:v 0}))) (def indexOfStr_i 0) (while (<= indexOfStr_i (- indexOfStr_hlen indexOfStr_nlen)) (do (when (= (subs indexOfStr_h indexOfStr_i (+ indexOfStr_i indexOfStr_nlen)) indexOfStr_n) (throw (ex-info "return" {:v indexOfStr_i}))) (def indexOfStr_i (+ indexOfStr_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn stringSearchSingle [stringSearchSingle_h stringSearchSingle_n]
  (try (throw (ex-info "return" {:v (indexOfStr stringSearchSingle_h stringSearchSingle_n)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn stringSearch [stringSearch_h stringSearch_n]
  (try (do (def stringSearch_result []) (def stringSearch_start 0) (def stringSearch_hlen (count stringSearch_h)) (def stringSearch_nlen (count stringSearch_n)) (loop [while_flag_1 true] (when (and while_flag_1 (< stringSearch_start stringSearch_hlen)) (do (def stringSearch_idx (indexOfStr (subs stringSearch_h stringSearch_start stringSearch_hlen) stringSearch_n)) (if (>= stringSearch_idx 0) (do (def stringSearch_result (conj stringSearch_result (+ stringSearch_start stringSearch_idx))) (def stringSearch_start (+ (+ stringSearch_start stringSearch_idx) stringSearch_nlen)) (recur while_flag_1)) (recur false))))) (throw (ex-info "return" {:v stringSearch_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn display [display_nums]
  (try (do (def display_s "[") (def display_i 0) (while (< display_i (count display_nums)) (do (when (> display_i 0) (def display_s (str display_s ", "))) (def display_s (str display_s (str (nth display_nums display_i)))) (def display_i (+ display_i 1)))) (def display_s (str display_s "]")) (throw (ex-info "return" {:v display_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_texts ["GCTAGCTCTACGAGTCTA" "GGCTATAATGCGTA" "there would have been a time for such a word" "needle need noodle needle" "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages" "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."]) (def main_patterns ["TCTA" "TAATAAA" "word" "needle" "and" "alfalfa"]) (def main_i 0) (while (< main_i (count main_texts)) (do (println (str (str (str "text" (str (+ main_i 1))) " = ") (nth main_texts main_i))) (def main_i (+ main_i 1)))) (println "") (def main_j 0) (while (< main_j (count main_texts)) (do (def main_idxs (stringSearch (nth main_texts main_j) (nth main_patterns main_j))) (println (str (str (str (str (str "Found \"" (nth main_patterns main_j)) "\" in 'text") (str (+ main_j 1))) "' at indexes ") (display main_idxs))) (def main_j (+ main_j 1))))))

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
