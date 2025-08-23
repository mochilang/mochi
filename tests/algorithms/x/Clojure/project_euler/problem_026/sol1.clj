(ns main (:refer-clojure :exclude [contains solution main]))

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

(declare contains solution main)

(declare _read_file)

(def ^:dynamic contains_x nil)

(def ^:dynamic count_v nil)

(def ^:dynamic solution_d nil)

(def ^:dynamic solution_longest_len nil)

(def ^:dynamic solution_rem nil)

(def ^:dynamic solution_remainders nil)

(def ^:dynamic solution_the_digit nil)

(defn contains [contains_lst contains_val]
  (binding [contains_x nil] (try (do (doseq [contains_x contains_lst] (when (= contains_x contains_val) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_numerator solution_limit]
  (binding [count_v nil solution_d nil solution_longest_len nil solution_rem nil solution_remainders nil solution_the_digit nil] (try (do (set! solution_the_digit 1) (set! solution_longest_len 0) (doseq [solution_d (range solution_numerator solution_limit)] (do (set! solution_remainders []) (set! solution_rem solution_numerator) (set! count_v 1) (while (<= count_v solution_limit) (do (if (contains solution_remainders solution_rem) (when (< solution_longest_len (count solution_remainders)) (do (set! solution_longest_len (count solution_remainders)) (set! solution_the_digit solution_d))) (do (set! solution_remainders (conj solution_remainders solution_rem)) (set! solution_rem (mod (* solution_rem 10) solution_d)))) (set! count_v (+ count_v 1)))))) (throw (ex-info "return" {:v solution_the_digit}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (mochi_str (solution 1 10))) (println (mochi_str (solution 10 100))) (println (mochi_str (solution 10 1000)))))

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
