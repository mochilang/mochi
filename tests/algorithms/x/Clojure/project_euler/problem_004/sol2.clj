(ns main (:refer-clojure :exclude [is_palindrome solution]))

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

(declare is_palindrome solution)

(def ^:dynamic is_palindrome_n nil)

(def ^:dynamic is_palindrome_rev nil)

(def ^:dynamic solution_answer nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_product nil)

(defn is_palindrome [is_palindrome_num]
  (binding [is_palindrome_n nil is_palindrome_rev nil] (try (do (when (< is_palindrome_num 0) (throw (ex-info "return" {:v false}))) (set! is_palindrome_n is_palindrome_num) (set! is_palindrome_rev 0) (while (> is_palindrome_n 0) (do (set! is_palindrome_rev (+ (* is_palindrome_rev 10) (mod is_palindrome_n 10))) (set! is_palindrome_n (/ is_palindrome_n 10)))) (throw (ex-info "return" {:v (= is_palindrome_rev is_palindrome_num)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [solution_answer nil solution_i nil solution_j nil solution_product nil] (try (do (set! solution_answer 0) (set! solution_i 999) (while (>= solution_i 100) (do (set! solution_j 999) (while (>= solution_j 100) (do (set! solution_product (* solution_i solution_j)) (when (and (and (< solution_product solution_limit) (is_palindrome solution_product)) (> solution_product solution_answer)) (set! solution_answer solution_product)) (set! solution_j (- solution_j 1)))) (set! solution_i (- solution_i 1)))) (throw (ex-info "return" {:v solution_answer}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (solution 998001)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
