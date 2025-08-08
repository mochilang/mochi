(ns main (:refer-clojure :exclude [build_set word_break print_bool]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare build_set word_break print_bool)

(def ^:dynamic build_set_m nil)

(def ^:dynamic word_break_dict nil)

(def ^:dynamic word_break_dp nil)

(def ^:dynamic word_break_i nil)

(def ^:dynamic word_break_j nil)

(def ^:dynamic word_break_n nil)

(def ^:dynamic word_break_sub nil)

(defn build_set [build_set_words]
  (binding [build_set_m nil] (try (do (set! build_set_m {}) (doseq [w build_set_words] (set! build_set_m (assoc build_set_m w true))) (throw (ex-info "return" {:v build_set_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn word_break [word_break_s word_break_words]
  (binding [word_break_dict nil word_break_dp nil word_break_i nil word_break_j nil word_break_n nil word_break_sub nil] (try (do (set! word_break_n (count word_break_s)) (set! word_break_dict (build_set word_break_words)) (set! word_break_dp []) (set! word_break_i 0) (while (<= word_break_i word_break_n) (do (set! word_break_dp (conj word_break_dp false)) (set! word_break_i (+ word_break_i 1)))) (set! word_break_dp (assoc word_break_dp 0 true)) (set! word_break_i 1) (while (<= word_break_i word_break_n) (do (set! word_break_j 0) (while (< word_break_j word_break_i) (do (when (nth word_break_dp word_break_j) (do (set! word_break_sub (subs word_break_s word_break_j (min word_break_i (count word_break_s)))) (when (in word_break_sub word_break_dict) (do (set! word_break_dp (assoc word_break_dp word_break_i true)) (set! word_break_j word_break_i))))) (set! word_break_j (+ word_break_j 1)))) (set! word_break_i (+ word_break_i 1)))) (throw (ex-info "return" {:v (nth word_break_dp word_break_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_bool [print_bool_b]
  (if print_bool_b (println true) (println false)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_bool (word_break "applepenapple" ["apple" "pen"]))
      (print_bool (word_break "catsandog" ["cats" "dog" "sand" "and" "cat"]))
      (print_bool (word_break "cars" ["car" "ca" "rs"]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
