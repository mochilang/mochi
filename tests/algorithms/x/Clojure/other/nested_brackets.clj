(ns main (:refer-clojure :exclude [slice_without_last is_balanced main]))

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

(declare slice_without_last is_balanced main)

(def ^:dynamic is_balanced_i nil)

(def ^:dynamic is_balanced_stack nil)

(def ^:dynamic is_balanced_symbol nil)

(def ^:dynamic is_balanced_top nil)

(def ^:dynamic slice_without_last_i nil)

(def ^:dynamic slice_without_last_res nil)

(def ^:dynamic main_OPEN_TO_CLOSED {"(" ")" "[" "]" "{" "}"})

(defn slice_without_last [slice_without_last_xs]
  (binding [slice_without_last_i nil slice_without_last_res nil] (try (do (set! slice_without_last_res []) (set! slice_without_last_i 0) (while (< slice_without_last_i (- (count slice_without_last_xs) 1)) (do (set! slice_without_last_res (conj slice_without_last_res (nth slice_without_last_xs slice_without_last_i))) (set! slice_without_last_i (+ slice_without_last_i 1)))) (throw (ex-info "return" {:v slice_without_last_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_balanced [is_balanced_s]
  (binding [is_balanced_i nil is_balanced_stack nil is_balanced_symbol nil is_balanced_top nil] (try (do (set! is_balanced_stack []) (set! is_balanced_i 0) (while (< is_balanced_i (count is_balanced_s)) (do (set! is_balanced_symbol (subs is_balanced_s is_balanced_i (min (+ is_balanced_i 1) (count is_balanced_s)))) (if (in is_balanced_symbol main_OPEN_TO_CLOSED) (set! is_balanced_stack (conj is_balanced_stack is_balanced_symbol)) (when (or (or (= is_balanced_symbol ")") (= is_balanced_symbol "]")) (= is_balanced_symbol "}")) (do (when (= (count is_balanced_stack) 0) (throw (ex-info "return" {:v false}))) (set! is_balanced_top (nth is_balanced_stack (- (count is_balanced_stack) 1))) (when (not= (get main_OPEN_TO_CLOSED is_balanced_top) is_balanced_symbol) (throw (ex-info "return" {:v false}))) (set! is_balanced_stack (slice_without_last is_balanced_stack))))) (set! is_balanced_i (+ is_balanced_i 1)))) (throw (ex-info "return" {:v (= (count is_balanced_stack) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (is_balanced "")) (println (is_balanced "()")) (println (is_balanced "[]")) (println (is_balanced "{}")) (println (is_balanced "()[]{}")) (println (is_balanced "(())")) (println (is_balanced "[[")) (println (is_balanced "([{}])")) (println (is_balanced "(()[)]")) (println (is_balanced "([)]")) (println (is_balanced "[[()]]")) (println (is_balanced "(()(()))")) (println (is_balanced "]")) (println (is_balanced "Life is a bowl of cherries.")) (println (is_balanced "Life is a bowl of che{}ies.")) (println (is_balanced "Life is a bowl of che}{ies."))))

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
