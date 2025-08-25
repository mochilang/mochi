(ns main (:refer-clojure :exclude [floyd reverse_floyd pretty_print main]))

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

(declare floyd reverse_floyd pretty_print main)

(declare _read_file)

(def ^:dynamic floyd_i nil)

(def ^:dynamic floyd_j nil)

(def ^:dynamic floyd_k nil)

(def ^:dynamic floyd_result nil)

(def ^:dynamic pretty_print_lower_half nil)

(def ^:dynamic pretty_print_upper_half nil)

(def ^:dynamic reverse_floyd_i nil)

(def ^:dynamic reverse_floyd_j nil)

(def ^:dynamic reverse_floyd_k nil)

(def ^:dynamic reverse_floyd_result nil)

(defn floyd [floyd_n]
  (binding [floyd_i nil floyd_j nil floyd_k nil floyd_result nil] (try (do (set! floyd_result "") (set! floyd_i 0) (while (< floyd_i floyd_n) (do (set! floyd_j 0) (while (< floyd_j (- (- floyd_n floyd_i) 1)) (do (set! floyd_result (str floyd_result " ")) (set! floyd_j (+' floyd_j 1)))) (set! floyd_k 0) (while (< floyd_k (+' floyd_i 1)) (do (set! floyd_result (str floyd_result "* ")) (set! floyd_k (+' floyd_k 1)))) (set! floyd_result (str floyd_result "\n")) (set! floyd_i (+' floyd_i 1)))) (throw (ex-info "return" {:v floyd_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_floyd [reverse_floyd_n]
  (binding [reverse_floyd_i nil reverse_floyd_j nil reverse_floyd_k nil reverse_floyd_result nil] (try (do (set! reverse_floyd_result "") (set! reverse_floyd_i reverse_floyd_n) (while (> reverse_floyd_i 0) (do (set! reverse_floyd_j reverse_floyd_i) (while (> reverse_floyd_j 0) (do (set! reverse_floyd_result (str reverse_floyd_result "* ")) (set! reverse_floyd_j (- reverse_floyd_j 1)))) (set! reverse_floyd_result (str reverse_floyd_result "\n")) (set! reverse_floyd_k (+' (- reverse_floyd_n reverse_floyd_i) 1)) (while (> reverse_floyd_k 0) (do (set! reverse_floyd_result (str reverse_floyd_result " ")) (set! reverse_floyd_k (- reverse_floyd_k 1)))) (set! reverse_floyd_i (- reverse_floyd_i 1)))) (throw (ex-info "return" {:v reverse_floyd_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pretty_print [pretty_print_n]
  (binding [pretty_print_lower_half nil pretty_print_upper_half nil] (try (do (when (<= pretty_print_n 0) (throw (ex-info "return" {:v "       ...       ....        nothing printing :("}))) (set! pretty_print_upper_half (floyd pretty_print_n)) (set! pretty_print_lower_half (reverse_floyd pretty_print_n)) (throw (ex-info "return" {:v (str pretty_print_upper_half pretty_print_lower_half)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (pretty_print 3)) (println (pretty_print 0))))

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
