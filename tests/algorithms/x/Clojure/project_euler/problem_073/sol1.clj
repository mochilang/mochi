(ns main (:refer-clojure :exclude [gcd solution main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare gcd solution main)

(declare _read_file)

(def ^:dynamic gcd_temp nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic solution_d nil)

(def ^:dynamic solution_fractions_number nil)

(def ^:dynamic solution_half nil)

(def ^:dynamic solution_n nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_temp nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_temp (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_temp))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_max_d]
  (binding [solution_d nil solution_fractions_number nil solution_half nil solution_n nil] (try (do (set! solution_fractions_number 0) (set! solution_d 0) (while (<= solution_d solution_max_d) (do (set! solution_n (+ (/ solution_d 3) 1)) (set! solution_half (/ (+ solution_d 1) 2)) (while (< solution_n solution_half) (do (when (= (gcd solution_n solution_d) 1) (set! solution_fractions_number (+ solution_fractions_number 1))) (set! solution_n (+ solution_n 1)))) (set! solution_d (+ solution_d 1)))) (throw (ex-info "return" {:v solution_fractions_number}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (solution 12000)))

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
