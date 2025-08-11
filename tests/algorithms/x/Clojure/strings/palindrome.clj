(ns main (:refer-clojure :exclude [reverse is_palindrome is_palindrome_traversal is_palindrome_recursive is_palindrome_slice main]))

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

(declare reverse is_palindrome is_palindrome_traversal is_palindrome_recursive is_palindrome_slice main)

(def ^:dynamic is_palindrome_end_i nil)

(def ^:dynamic is_palindrome_start_i nil)

(def ^:dynamic is_palindrome_traversal_end nil)

(def ^:dynamic is_palindrome_traversal_i nil)

(def ^:dynamic is_palindrome_traversal_n nil)

(def ^:dynamic main_expected nil)

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_r3 nil)

(def ^:dynamic main_r4 nil)

(def ^:dynamic main_s nil)

(def ^:dynamic reverse_i nil)

(def ^:dynamic reverse_res nil)

(defn reverse [reverse_s]
  (binding [reverse_i nil reverse_res nil] (try (do (set! reverse_res "") (set! reverse_i (- (count reverse_s) 1)) (while (>= reverse_i 0) (do (set! reverse_res (str reverse_res (subs reverse_s reverse_i (+ reverse_i 1)))) (set! reverse_i (- reverse_i 1)))) (throw (ex-info "return" {:v reverse_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_palindrome [is_palindrome_s]
  (binding [is_palindrome_end_i nil is_palindrome_start_i nil] (try (do (set! is_palindrome_start_i 0) (set! is_palindrome_end_i (- (count is_palindrome_s) 1)) (while (< is_palindrome_start_i is_palindrome_end_i) (if (= (subs is_palindrome_s is_palindrome_start_i (+ is_palindrome_start_i 1)) (subs is_palindrome_s is_palindrome_end_i (+ is_palindrome_end_i 1))) (do (set! is_palindrome_start_i (+ is_palindrome_start_i 1)) (set! is_palindrome_end_i (- is_palindrome_end_i 1))) (throw (ex-info "return" {:v false})))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_palindrome_traversal [is_palindrome_traversal_s]
  (binding [is_palindrome_traversal_end nil is_palindrome_traversal_i nil is_palindrome_traversal_n nil] (try (do (set! is_palindrome_traversal_end (/ (count is_palindrome_traversal_s) 2)) (set! is_palindrome_traversal_n (count is_palindrome_traversal_s)) (set! is_palindrome_traversal_i 0) (while (< is_palindrome_traversal_i is_palindrome_traversal_end) (do (when (not= (subs is_palindrome_traversal_s is_palindrome_traversal_i (+ is_palindrome_traversal_i 1)) (subs is_palindrome_traversal_s (- (- is_palindrome_traversal_n is_palindrome_traversal_i) 1) (+ (- (- is_palindrome_traversal_n is_palindrome_traversal_i) 1) 1))) (throw (ex-info "return" {:v false}))) (set! is_palindrome_traversal_i (+ is_palindrome_traversal_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_palindrome_recursive [is_palindrome_recursive_s]
  (try (do (when (<= (count is_palindrome_recursive_s) 1) (throw (ex-info "return" {:v true}))) (if (= (subs is_palindrome_recursive_s 0 (+ 0 1)) (subs is_palindrome_recursive_s (- (count is_palindrome_recursive_s) 1) (+ (- (count is_palindrome_recursive_s) 1) 1))) (is_palindrome_recursive (subs is_palindrome_recursive_s 1 (min (- (count is_palindrome_recursive_s) 1) (count is_palindrome_recursive_s)))) false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_palindrome_slice [is_palindrome_slice_s]
  (try (throw (ex-info "return" {:v (= is_palindrome_slice_s (reverse is_palindrome_slice_s))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_test_data [{:expected true :text "MALAYALAM"} {:expected false :text "String"} {:expected true :text "rotor"} {:expected true :text "level"} {:expected true :text "A"} {:expected true :text "BB"} {:expected false :text "ABC"} {:expected true :text "amanaplanacanalpanama"}])

(defn main []
  (binding [main_expected nil main_r1 nil main_r2 nil main_r3 nil main_r4 nil main_s nil] (do (doseq [t main_test_data] (do (set! main_s (:text t)) (set! main_expected (:expected t)) (set! main_r1 (is_palindrome main_s)) (set! main_r2 (is_palindrome_traversal main_s)) (set! main_r3 (is_palindrome_recursive main_s)) (set! main_r4 (is_palindrome_slice main_s)) (when (or (or (or (not= main_r1 main_expected) (not= main_r2 main_expected)) (not= main_r3 main_expected)) (not= main_r4 main_expected)) (throw (Exception. "algorithm mismatch"))) (println (str (str main_s " ") (str main_expected))))) (println "a man a plan a canal panama"))))

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
