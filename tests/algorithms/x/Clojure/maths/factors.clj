(ns main (:refer-clojure :exclude [reverse factors_of_a_number run_tests main]))

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

(declare reverse factors_of_a_number run_tests main)

(def ^:dynamic factors_of_a_number_d nil)

(def ^:dynamic factors_of_a_number_facs nil)

(def ^:dynamic factors_of_a_number_i nil)

(def ^:dynamic factors_of_a_number_large nil)

(def ^:dynamic factors_of_a_number_small nil)

(def ^:dynamic reverse_i nil)

(def ^:dynamic reverse_res nil)

(defn reverse [reverse_xs]
  (binding [reverse_i nil reverse_res nil] (try (do (set! reverse_res []) (set! reverse_i (- (count reverse_xs) 1)) (while (>= reverse_i 0) (do (set! reverse_res (conj reverse_res (nth reverse_xs reverse_i))) (set! reverse_i (- reverse_i 1)))) (throw (ex-info "return" {:v reverse_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factors_of_a_number [factors_of_a_number_num]
  (binding [factors_of_a_number_d nil factors_of_a_number_facs nil factors_of_a_number_i nil factors_of_a_number_large nil factors_of_a_number_small nil] (try (do (set! factors_of_a_number_facs []) (when (< factors_of_a_number_num 1) (throw (ex-info "return" {:v factors_of_a_number_facs}))) (set! factors_of_a_number_small []) (set! factors_of_a_number_large []) (set! factors_of_a_number_i 1) (while (<= (* factors_of_a_number_i factors_of_a_number_i) factors_of_a_number_num) (do (when (= (mod factors_of_a_number_num factors_of_a_number_i) 0) (do (set! factors_of_a_number_small (conj factors_of_a_number_small factors_of_a_number_i)) (set! factors_of_a_number_d (quot factors_of_a_number_num factors_of_a_number_i)) (when (not= factors_of_a_number_d factors_of_a_number_i) (set! factors_of_a_number_large (conj factors_of_a_number_large factors_of_a_number_d))))) (set! factors_of_a_number_i (+ factors_of_a_number_i 1)))) (set! factors_of_a_number_facs (concat factors_of_a_number_small (reverse factors_of_a_number_large))) (throw (ex-info "return" {:v factors_of_a_number_facs}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_tests []
  (do (when (not= (factors_of_a_number 1) [1]) (throw (Exception. "case1 failed"))) (when (not= (factors_of_a_number 5) [1 5]) (throw (Exception. "case2 failed"))) (when (not= (factors_of_a_number 24) [1 2 3 4 6 8 12 24]) (throw (Exception. "case3 failed"))) (when (not= (factors_of_a_number (- 24)) []) (throw (Exception. "case4 failed")))))

(defn main []
  (do (run_tests) (println (str (factors_of_a_number 24)))))

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
