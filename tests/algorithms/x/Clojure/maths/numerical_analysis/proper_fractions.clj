(ns main (:refer-clojure :exclude [gcd proper_fractions test_proper_fractions main]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare gcd proper_fractions test_proper_fractions main)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic proper_fractions_n nil)

(def ^:dynamic proper_fractions_res nil)

(def ^:dynamic test_proper_fractions_a nil)

(def ^:dynamic test_proper_fractions_b nil)

(def ^:dynamic test_proper_fractions_c nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (if (< gcd_x 0) (- gcd_x) gcd_x)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn proper_fractions [proper_fractions_den]
  (binding [proper_fractions_n nil proper_fractions_res nil] (try (do (when (< proper_fractions_den 0) (throw (Exception. "The Denominator Cannot be less than 0"))) (set! proper_fractions_res []) (set! proper_fractions_n 1) (while (< proper_fractions_n proper_fractions_den) (do (when (= (gcd proper_fractions_n proper_fractions_den) 1) (set! proper_fractions_res (conj proper_fractions_res (str (str (str proper_fractions_n) "/") (str proper_fractions_den))))) (set! proper_fractions_n (+ proper_fractions_n 1)))) (throw (ex-info "return" {:v proper_fractions_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_proper_fractions []
  (binding [test_proper_fractions_a nil test_proper_fractions_b nil test_proper_fractions_c nil] (do (set! test_proper_fractions_a (proper_fractions 10)) (when (not= test_proper_fractions_a ["1/10" "3/10" "7/10" "9/10"]) (throw (Exception. "test 10 failed"))) (set! test_proper_fractions_b (proper_fractions 5)) (when (not= test_proper_fractions_b ["1/5" "2/5" "3/5" "4/5"]) (throw (Exception. "test 5 failed"))) (set! test_proper_fractions_c (proper_fractions 0)) (when (not= test_proper_fractions_c []) (throw (Exception. "test 0 failed"))))))

(defn main []
  (do (test_proper_fractions) (println (str (proper_fractions 10))) (println (str (proper_fractions 5))) (println (str (proper_fractions 0)))))

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
