(ns main (:refer-clojure :exclude [one_pence two_pence five_pence ten_pence twenty_pence fifty_pence one_pound two_pound solution main]))

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

(declare one_pence two_pence five_pence ten_pence twenty_pence fifty_pence one_pound two_pound solution main)

(declare _read_file)

(def ^:dynamic main_n nil)

(defn one_pence []
  (try (throw (ex-info "return" {:v 1})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn two_pence [two_pence_x]
  (try (if (< two_pence_x 0) 0 (+ (two_pence (- two_pence_x 2)) (one_pence))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn five_pence [five_pence_x]
  (try (if (< five_pence_x 0) 0 (+ (five_pence (- five_pence_x 5)) (two_pence five_pence_x))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ten_pence [ten_pence_x]
  (try (if (< ten_pence_x 0) 0 (+ (ten_pence (- ten_pence_x 10)) (five_pence ten_pence_x))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn twenty_pence [twenty_pence_x]
  (try (if (< twenty_pence_x 0) 0 (+ (twenty_pence (- twenty_pence_x 20)) (ten_pence twenty_pence_x))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fifty_pence [fifty_pence_x]
  (try (if (< fifty_pence_x 0) 0 (+ (fifty_pence (- fifty_pence_x 50)) (twenty_pence fifty_pence_x))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn one_pound [one_pound_x]
  (try (if (< one_pound_x 0) 0 (+ (one_pound (- one_pound_x 100)) (fifty_pence one_pound_x))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn two_pound [two_pound_x]
  (try (if (< two_pound_x 0) 0 (+ (two_pound (- two_pound_x 200)) (one_pound two_pound_x))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn solution [solution_n]
  (try (throw (ex-info "return" {:v (two_pound solution_n)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_n nil] (do (set! main_n (toi (read-line))) (println (mochi_str (solution main_n))))))

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
