(ns main (:refer-clojure :exclude [rand randint fisher_yates_shuffle_int fisher_yates_shuffle_str]))

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

(declare rand randint fisher_yates_shuffle_int fisher_yates_shuffle_str)

(def ^:dynamic fisher_yates_shuffle_int_a nil)

(def ^:dynamic fisher_yates_shuffle_int_b nil)

(def ^:dynamic fisher_yates_shuffle_int_i nil)

(def ^:dynamic fisher_yates_shuffle_int_res nil)

(def ^:dynamic fisher_yates_shuffle_int_temp nil)

(def ^:dynamic fisher_yates_shuffle_str_a nil)

(def ^:dynamic fisher_yates_shuffle_str_b nil)

(def ^:dynamic fisher_yates_shuffle_str_i nil)

(def ^:dynamic fisher_yates_shuffle_str_res nil)

(def ^:dynamic fisher_yates_shuffle_str_temp nil)

(def ^:dynamic randint_r nil)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v (quot main_seed 65536)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn randint [randint_a randint_b]
  (binding [randint_r nil] (try (do (set! randint_r (rand)) (throw (ex-info "return" {:v (+ randint_a (mod randint_r (+ (- randint_b randint_a) 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fisher_yates_shuffle_int [fisher_yates_shuffle_int_data]
  (binding [fisher_yates_shuffle_int_a nil fisher_yates_shuffle_int_b nil fisher_yates_shuffle_int_i nil fisher_yates_shuffle_int_res nil fisher_yates_shuffle_int_temp nil] (try (do (set! fisher_yates_shuffle_int_res fisher_yates_shuffle_int_data) (set! fisher_yates_shuffle_int_i 0) (while (< fisher_yates_shuffle_int_i (count fisher_yates_shuffle_int_res)) (do (set! fisher_yates_shuffle_int_a (randint 0 (- (count fisher_yates_shuffle_int_res) 1))) (set! fisher_yates_shuffle_int_b (randint 0 (- (count fisher_yates_shuffle_int_res) 1))) (set! fisher_yates_shuffle_int_temp (nth fisher_yates_shuffle_int_res fisher_yates_shuffle_int_a)) (set! fisher_yates_shuffle_int_res (assoc fisher_yates_shuffle_int_res fisher_yates_shuffle_int_a (nth fisher_yates_shuffle_int_res fisher_yates_shuffle_int_b))) (set! fisher_yates_shuffle_int_res (assoc fisher_yates_shuffle_int_res fisher_yates_shuffle_int_b fisher_yates_shuffle_int_temp)) (set! fisher_yates_shuffle_int_i (+ fisher_yates_shuffle_int_i 1)))) (throw (ex-info "return" {:v fisher_yates_shuffle_int_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fisher_yates_shuffle_str [fisher_yates_shuffle_str_data]
  (binding [fisher_yates_shuffle_str_a nil fisher_yates_shuffle_str_b nil fisher_yates_shuffle_str_i nil fisher_yates_shuffle_str_res nil fisher_yates_shuffle_str_temp nil] (try (do (set! fisher_yates_shuffle_str_res fisher_yates_shuffle_str_data) (set! fisher_yates_shuffle_str_i 0) (while (< fisher_yates_shuffle_str_i (count fisher_yates_shuffle_str_res)) (do (set! fisher_yates_shuffle_str_a (randint 0 (- (count fisher_yates_shuffle_str_res) 1))) (set! fisher_yates_shuffle_str_b (randint 0 (- (count fisher_yates_shuffle_str_res) 1))) (set! fisher_yates_shuffle_str_temp (nth fisher_yates_shuffle_str_res fisher_yates_shuffle_str_a)) (set! fisher_yates_shuffle_str_res (assoc fisher_yates_shuffle_str_res fisher_yates_shuffle_str_a (nth fisher_yates_shuffle_str_res fisher_yates_shuffle_str_b))) (set! fisher_yates_shuffle_str_res (assoc fisher_yates_shuffle_str_res fisher_yates_shuffle_str_b fisher_yates_shuffle_str_temp)) (set! fisher_yates_shuffle_str_i (+ fisher_yates_shuffle_str_i 1)))) (throw (ex-info "return" {:v fisher_yates_shuffle_str_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_integers [0 1 2 3 4 5 6 7])

(def ^:dynamic main_strings ["python" "says" "hello" "!"])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Fisher-Yates Shuffle:")
      (println (str (str (str "List " (str main_integers)) " ") (str main_strings)))
      (println (str (str (str "FY Shuffle " (str (fisher_yates_shuffle_int main_integers))) " ") (str (fisher_yates_shuffle_str main_strings))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
