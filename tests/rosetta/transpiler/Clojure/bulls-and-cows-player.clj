(ns main (:refer-clojure :exclude [indexOf fields makePatterns main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOf fields makePatterns main)

(declare fields_ch fields_cur fields_i fields_words indexOf_i main_b main_bulls main_c main_cg main_cows main_cp main_guess main_i main_idx main_line main_pat main_patterns main_toks makePatterns_digits makePatterns_i makePatterns_j makePatterns_k makePatterns_l makePatterns_pats next_v)

(defn indexOf [indexOf_s indexOf_ch]
  (try (do (def indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (+ indexOf_i 1)) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (def indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fields [fields_s]
  (try (do (def fields_words []) (def fields_cur "") (def fields_i 0) (while (< fields_i (count fields_s)) (do (def fields_ch (subs fields_s fields_i (+ fields_i 1))) (if (or (or (= fields_ch " ") (= fields_ch "\t")) (= fields_ch "\n")) (when (> (count fields_cur) 0) (do (def fields_words (conj fields_words fields_cur)) (def fields_cur ""))) (def fields_cur (str fields_cur fields_ch))) (def fields_i (+ fields_i 1)))) (when (> (count fields_cur) 0) (def fields_words (conj fields_words fields_cur))) (throw (ex-info "return" {:v fields_words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn makePatterns []
  (try (do (def makePatterns_digits ["1" "2" "3" "4" "5" "6" "7" "8" "9"]) (def makePatterns_pats []) (def makePatterns_i 0) (while (< makePatterns_i (count makePatterns_digits)) (do (def makePatterns_j 0) (while (< makePatterns_j (count makePatterns_digits)) (do (when (not= makePatterns_j makePatterns_i) (do (def makePatterns_k 0) (while (< makePatterns_k (count makePatterns_digits)) (do (when (and (not= makePatterns_k makePatterns_i) (not= makePatterns_k makePatterns_j)) (do (def makePatterns_l 0) (while (< makePatterns_l (count makePatterns_digits)) (do (when (and (and (not= makePatterns_l makePatterns_i) (not= makePatterns_l makePatterns_j)) (not= makePatterns_l makePatterns_k)) (def makePatterns_pats (conj makePatterns_pats (str (str (str (nth makePatterns_digits makePatterns_i) (nth makePatterns_digits makePatterns_j)) (nth makePatterns_digits makePatterns_k)) (nth makePatterns_digits makePatterns_l))))) (def makePatterns_l (+ makePatterns_l 1)))))) (def makePatterns_k (+ makePatterns_k 1)))))) (def makePatterns_j (+ makePatterns_j 1)))) (def makePatterns_i (+ makePatterns_i 1)))) (throw (ex-info "return" {:v makePatterns_pats}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (try (do (println (str (str (str (str (str "Cows and bulls/player\n" "You think of four digit number of unique digits in the range 1 to 9.\n") "I guess.  You score my guess:\n") "    A correct digit but not in the correct place is a cow.\n") "    A correct digit in the correct place is a bull.\n") "You give my score as two numbers separated with a space.")) (def main_patterns (makePatterns)) (while true (do (when (= (count main_patterns) 0) (do (println "Oops, check scoring.") (throw (ex-info "return" {:v nil})))) (def main_guess (nth main_patterns 0)) (def main_patterns (subvec main_patterns 1 (count main_patterns))) (def main_cows 0) (def main_bulls 0) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (println (str (str "My guess: " main_guess) ".  Score? (c b) ")) (def main_line (read-line)) (def main_toks (fields main_line)) (cond (= (count main_toks) 2) (do (def main_c (Integer/parseInt (nth main_toks 0))) (def main_b (Integer/parseInt (nth main_toks 1))) (when (and (and (and (and (>= main_c 0) (<= main_c 4)) (>= main_b 0)) (<= main_b 4)) (<= (+ main_c main_b) 4)) (do (def main_cows main_c) (def main_bulls main_b) (recur false)))) :else (do (println "Score guess as two numbers: cows bulls") (recur while_flag_1)))))) (when (= main_bulls 4) (do (println "I did it. :)") (throw (ex-info "return" {:v nil})))) (def next_v []) (def main_idx 0) (while (< main_idx (count main_patterns)) (do (def main_pat (nth main_patterns main_idx)) (def main_c 0) (def main_b 0) (def main_i 0) (while (< main_i 4) (do (def main_cg (subs main_guess main_i (+ main_i 1))) (def main_cp (subs main_pat main_i (+ main_i 1))) (if (= main_cg main_cp) (def main_b (+ main_b 1)) (when (>= (indexOf main_pat main_cg) 0) (def main_c (+ main_c 1)))) (def main_i (+ main_i 1)))) (when (and (= main_c main_cows) (= main_b main_bulls)) (def next_v (conj next_v main_pat))) (def main_idx (+ main_idx 1)))) (def main_patterns next_v)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

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
