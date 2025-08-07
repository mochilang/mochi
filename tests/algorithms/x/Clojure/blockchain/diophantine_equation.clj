(ns main (:refer-clojure :exclude [gcd extended_gcd diophantine diophantine_all_soln]))

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

(declare gcd extended_gcd diophantine diophantine_all_soln)

(def ^:dynamic diophantine_all_soln_base nil)

(def ^:dynamic diophantine_all_soln_d nil)

(def ^:dynamic diophantine_all_soln_i nil)

(def ^:dynamic diophantine_all_soln_p nil)

(def ^:dynamic diophantine_all_soln_q nil)

(def ^:dynamic diophantine_all_soln_sols nil)

(def ^:dynamic diophantine_all_soln_x nil)

(def ^:dynamic diophantine_all_soln_x0 nil)

(def ^:dynamic diophantine_all_soln_y nil)

(def ^:dynamic diophantine_all_soln_y0 nil)

(def ^:dynamic diophantine_d nil)

(def ^:dynamic diophantine_eg nil)

(def ^:dynamic diophantine_r nil)

(def ^:dynamic diophantine_x nil)

(def ^:dynamic diophantine_y nil)

(def ^:dynamic extended_gcd_d nil)

(def ^:dynamic extended_gcd_p nil)

(def ^:dynamic extended_gcd_q nil)

(def ^:dynamic extended_gcd_res nil)

(def ^:dynamic extended_gcd_x nil)

(def ^:dynamic extended_gcd_y nil)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic main_j nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x (if (< gcd_a 0) (- gcd_a) gcd_a)) (set! gcd_y (if (< gcd_b 0) (- gcd_b) gcd_b)) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extended_gcd [extended_gcd_a extended_gcd_b]
  (binding [extended_gcd_d nil extended_gcd_p nil extended_gcd_q nil extended_gcd_res nil extended_gcd_x nil extended_gcd_y nil] (try (do (when (= extended_gcd_b 0) (throw (ex-info "return" {:v [extended_gcd_a 1 0]}))) (set! extended_gcd_res (extended_gcd extended_gcd_b (mod extended_gcd_a extended_gcd_b))) (set! extended_gcd_d (nth extended_gcd_res 0)) (set! extended_gcd_p (nth extended_gcd_res 1)) (set! extended_gcd_q (nth extended_gcd_res 2)) (set! extended_gcd_x extended_gcd_q) (set! extended_gcd_y (- extended_gcd_p (* extended_gcd_q (/ extended_gcd_a extended_gcd_b)))) (throw (ex-info "return" {:v [extended_gcd_d extended_gcd_x extended_gcd_y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn diophantine [diophantine_a diophantine_b diophantine_c]
  (binding [diophantine_d nil diophantine_eg nil diophantine_r nil diophantine_x nil diophantine_y nil] (try (do (set! diophantine_d (gcd diophantine_a diophantine_b)) (when (not= (mod diophantine_c diophantine_d) 0) (throw (Exception. "No solution"))) (set! diophantine_eg (extended_gcd diophantine_a diophantine_b)) (set! diophantine_r (/ diophantine_c diophantine_d)) (set! diophantine_x (* (nth diophantine_eg 1) diophantine_r)) (set! diophantine_y (* (nth diophantine_eg 2) diophantine_r)) (throw (ex-info "return" {:v [diophantine_x diophantine_y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn diophantine_all_soln [diophantine_all_soln_a diophantine_all_soln_b diophantine_all_soln_c diophantine_all_soln_n]
  (binding [diophantine_all_soln_base nil diophantine_all_soln_d nil diophantine_all_soln_i nil diophantine_all_soln_p nil diophantine_all_soln_q nil diophantine_all_soln_sols nil diophantine_all_soln_x nil diophantine_all_soln_x0 nil diophantine_all_soln_y nil diophantine_all_soln_y0 nil] (try (do (set! diophantine_all_soln_base (diophantine diophantine_all_soln_a diophantine_all_soln_b diophantine_all_soln_c)) (set! diophantine_all_soln_x0 (nth diophantine_all_soln_base 0)) (set! diophantine_all_soln_y0 (nth diophantine_all_soln_base 1)) (set! diophantine_all_soln_d (gcd diophantine_all_soln_a diophantine_all_soln_b)) (set! diophantine_all_soln_p (/ diophantine_all_soln_a diophantine_all_soln_d)) (set! diophantine_all_soln_q (/ diophantine_all_soln_b diophantine_all_soln_d)) (set! diophantine_all_soln_sols []) (set! diophantine_all_soln_i 0) (while (< diophantine_all_soln_i diophantine_all_soln_n) (do (set! diophantine_all_soln_x (+ diophantine_all_soln_x0 (* diophantine_all_soln_i diophantine_all_soln_q))) (set! diophantine_all_soln_y (- diophantine_all_soln_y0 (* diophantine_all_soln_i diophantine_all_soln_p))) (set! diophantine_all_soln_sols (conj diophantine_all_soln_sols [diophantine_all_soln_x diophantine_all_soln_y])) (set! diophantine_all_soln_i (+ diophantine_all_soln_i 1)))) (throw (ex-info "return" {:v diophantine_all_soln_sols}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_s1 (diophantine 10 6 14))

(def ^:dynamic main_sols (diophantine_all_soln 10 6 14 4))

(def ^:dynamic main_j 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_s1))
      (while (< main_j (count main_sols)) (do (println (str (nth main_sols main_j))) (def main_j (+ main_j 1))))
      (println (str (diophantine 391 299 (- 69))))
      (println (str (extended_gcd 10 6)))
      (println (str (extended_gcd 7 5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
