(ns main (:refer-clojure :exclude [factorial nth_permutation solution main]))

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

(declare factorial nth_permutation solution main)

(declare _read_file)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(def ^:dynamic nth_permutation_chars nil)

(def ^:dynamic nth_permutation_f nil)

(def ^:dynamic nth_permutation_k nil)

(def ^:dynamic nth_permutation_n nil)

(def ^:dynamic nth_permutation_pos nil)

(def ^:dynamic nth_permutation_res nil)

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (set! factorial_result 1) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_result (* factorial_result factorial_i)) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nth_permutation [nth_permutation_digits nth_permutation_index]
  (binding [nth_permutation_chars nil nth_permutation_f nil nth_permutation_k nil nth_permutation_n nil nth_permutation_pos nil nth_permutation_res nil] (try (do (set! nth_permutation_chars nth_permutation_digits) (set! nth_permutation_n nth_permutation_index) (set! nth_permutation_res "") (set! nth_permutation_k (count nth_permutation_chars)) (while (> nth_permutation_k 0) (do (set! nth_permutation_f (factorial (- nth_permutation_k 1))) (set! nth_permutation_pos (quot nth_permutation_n nth_permutation_f)) (set! nth_permutation_n (mod nth_permutation_n nth_permutation_f)) (set! nth_permutation_res (str nth_permutation_res (subs nth_permutation_chars nth_permutation_pos (min (+ nth_permutation_pos 1) (count nth_permutation_chars))))) (set! nth_permutation_chars (str (subs nth_permutation_chars 0 (min nth_permutation_pos (count nth_permutation_chars))) (subs nth_permutation_chars (+ nth_permutation_pos 1) (min (count nth_permutation_chars) (count nth_permutation_chars))))) (set! nth_permutation_k (- nth_permutation_k 1)))) (throw (ex-info "return" {:v nth_permutation_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (try (throw (ex-info "return" {:v (nth_permutation "0123456789" 999999)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (println (solution)))

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
