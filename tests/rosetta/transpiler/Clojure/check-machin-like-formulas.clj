(ns main (:refer-clojure :exclude [br format tanEval tans]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare br format tanEval tans)

(declare format_i format_s format_t main_testCases tanEval_a tanEval_b tanEval_ca tanEval_cb tans_a tans_b tans_half tans_t)

(defn br [br_n br_d]
  (try (throw (ex-info "return" {:v (/ br_n br_d)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_testCases [[{"a" 1 "n" 1 "d" 2} {"a" 1 "n" 1 "d" 3}] [{"a" 2 "n" 1 "d" 3} {"a" 1 "n" 1 "d" 7}] [{"a" 4 "n" 1 "d" 5} {"a" (- 1) "n" 1 "d" 239}] [{"a" 5 "n" 1 "d" 7} {"a" 2 "n" 3 "d" 79}] [{"a" 1 "n" 1 "d" 2} {"a" 1 "n" 1 "d" 5} {"a" 1 "n" 1 "d" 8}] [{"a" 4 "n" 1 "d" 5} {"a" (- 1) "n" 1 "d" 70} {"a" 1 "n" 1 "d" 99}] [{"a" 5 "n" 1 "d" 7} {"a" 4 "n" 1 "d" 53} {"a" 2 "n" 1 "d" 4443}] [{"a" 6 "n" 1 "d" 8} {"a" 2 "n" 1 "d" 57} {"a" 1 "n" 1 "d" 239}] [{"a" 8 "n" 1 "d" 10} {"a" (- 1) "n" 1 "d" 239} {"a" (- 4) "n" 1 "d" 515}] [{"a" 12 "n" 1 "d" 18} {"a" 8 "n" 1 "d" 57} {"a" (- 5) "n" 1 "d" 239}] [{"a" 16 "n" 1 "d" 21} {"a" 3 "n" 1 "d" 239} {"a" 4 "n" 3 "d" 1042}] [{"a" 22 "n" 1 "d" 28} {"a" 2 "n" 1 "d" 443} {"a" (- 5) "n" 1 "d" 1393} {"a" (- 10) "n" 1 "d" 11018}] [{"a" 22 "n" 1 "d" 38} {"a" 17 "n" 7 "d" 601} {"a" 10 "n" 7 "d" 8149}] [{"a" 44 "n" 1 "d" 57} {"a" 7 "n" 1 "d" 239} {"a" (- 12) "n" 1 "d" 682} {"a" 24 "n" 1 "d" 12943}] [{"a" 88 "n" 1 "d" 172} {"a" 51 "n" 1 "d" 239} {"a" 32 "n" 1 "d" 682} {"a" 44 "n" 1 "d" 5357} {"a" 68 "n" 1 "d" 12943}] [{"a" 88 "n" 1 "d" 172} {"a" 51 "n" 1 "d" 239} {"a" 32 "n" 1 "d" 682} {"a" 44 "n" 1 "d" 5357} {"a" 68 "n" 1 "d" 12944}]])

(defn format [format_ts]
  (try (do (def format_s "[") (def format_i 0) (while (< format_i (count format_ts)) (do (def format_t (nth format_ts format_i)) (def format_s (str (str (str (str (str (str (str format_s "{") (str (get format_t "a"))) " ") (str (get format_t "n"))) " ") (str (get format_t "d"))) "}")) (when (< format_i (- (count format_ts) 1)) (def format_s (str format_s " "))) (def format_i (+ format_i 1)))) (throw (ex-info "return" {:v (str format_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tanEval [tanEval_coef tanEval_f]
  (try (do (when (= tanEval_coef 1) (throw (ex-info "return" {:v tanEval_f}))) (when (< tanEval_coef 0) (throw (ex-info "return" {:v (- (tanEval (- tanEval_coef) tanEval_f))}))) (def tanEval_ca (quot tanEval_coef 2)) (def tanEval_cb (- tanEval_coef tanEval_ca)) (def tanEval_a (tanEval tanEval_ca tanEval_f)) (def tanEval_b (tanEval tanEval_cb tanEval_f)) (throw (ex-info "return" {:v (/ (+ tanEval_a tanEval_b) (- 1 (* tanEval_a tanEval_b)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tans [tans_m]
  (try (do (when (= (count tans_m) 1) (do (def tans_t (nth tans_m 0)) (throw (ex-info "return" {:v (tanEval (get tans_t "a") (br (get tans_t "n") (get tans_t "d")))})))) (def tans_half (quot (count tans_m) 2)) (def tans_a (tans (subvec tans_m 0 tans_half))) (def tans_b (tans (subvec tans_m tans_half (count tans_m)))) (throw (ex-info "return" {:v (/ (+ tans_a tans_b) (- 1 (* tans_a tans_b)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [ts main_testCases] (println (str (str (str "tan " (format ts)) " = ") (str (tans ts)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
