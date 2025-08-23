(ns main (:refer-clojure :exclude [factorial split_and_add solution]))

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

(declare factorial split_and_add solution)

(declare _read_file)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_res nil)

(def ^:dynamic solution_nfact nil)

(def ^:dynamic split_and_add_last nil)

(def ^:dynamic split_and_add_n nil)

(def ^:dynamic split_and_add_sum nil)

(defn factorial [factorial_num]
  (binding [factorial_i nil factorial_res nil] (try (do (set! factorial_res 1) (set! factorial_i 2) (while (<= factorial_i factorial_num) (do (set! factorial_res (* factorial_res factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split_and_add [split_and_add_number]
  (binding [split_and_add_last nil split_and_add_n nil split_and_add_sum nil] (try (do (set! split_and_add_sum 0) (set! split_and_add_n split_and_add_number) (while (> split_and_add_n 0) (do (set! split_and_add_last (mod split_and_add_n 10)) (set! split_and_add_sum (+ split_and_add_sum split_and_add_last)) (set! split_and_add_n (quot split_and_add_n 10)))) (throw (ex-info "return" {:v split_and_add_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_num]
  (binding [solution_nfact nil] (try (do (set! solution_nfact (factorial solution_num)) (throw (ex-info "return" {:v (split_and_add solution_nfact)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 100)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
