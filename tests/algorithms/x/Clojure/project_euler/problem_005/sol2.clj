(ns main (:refer-clojure :exclude [gcd lcm solution main]))

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

(declare gcd lcm solution main)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic solution_g nil)

(def ^:dynamic solution_i nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_t gcd_y) (set! gcd_y (mod gcd_x gcd_y)) (set! gcd_x gcd_t))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lcm [lcm_x lcm_y]
  (try (throw (ex-info "return" {:v (/ (* lcm_x lcm_y) (gcd lcm_x lcm_y))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn solution [solution_n]
  (binding [solution_g nil solution_i nil] (try (do (set! solution_g 1) (set! solution_i 1) (while (<= solution_i solution_n) (do (set! solution_g (lcm solution_g solution_i)) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (str (solution 20))))

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
