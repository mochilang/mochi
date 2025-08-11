(ns main (:refer-clojure :exclude [reverse max_int longest_palindromic_subsequence]))

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

(declare reverse max_int longest_palindromic_subsequence)

(def ^:dynamic longest_palindromic_subsequence_a_char nil)

(def ^:dynamic longest_palindromic_subsequence_b_char nil)

(def ^:dynamic longest_palindromic_subsequence_dp nil)

(def ^:dynamic longest_palindromic_subsequence_i nil)

(def ^:dynamic longest_palindromic_subsequence_j nil)

(def ^:dynamic longest_palindromic_subsequence_m nil)

(def ^:dynamic longest_palindromic_subsequence_n nil)

(def ^:dynamic longest_palindromic_subsequence_rev nil)

(def ^:dynamic longest_palindromic_subsequence_row nil)

(def ^:dynamic reverse_i nil)

(def ^:dynamic reverse_result nil)

(defn reverse [reverse_s]
  (binding [reverse_i nil reverse_result nil] (try (do (set! reverse_result "") (set! reverse_i (- (count reverse_s) 1)) (while (>= reverse_i 0) (do (set! reverse_result (str reverse_result (subs reverse_s reverse_i (min (+ reverse_i 1) (count reverse_s))))) (set! reverse_i (- reverse_i 1)))) (throw (ex-info "return" {:v reverse_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_int [max_int_a max_int_b]
  (try (if (> max_int_a max_int_b) max_int_a max_int_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn longest_palindromic_subsequence [longest_palindromic_subsequence_s]
  (binding [longest_palindromic_subsequence_a_char nil longest_palindromic_subsequence_b_char nil longest_palindromic_subsequence_dp nil longest_palindromic_subsequence_i nil longest_palindromic_subsequence_j nil longest_palindromic_subsequence_m nil longest_palindromic_subsequence_n nil longest_palindromic_subsequence_rev nil longest_palindromic_subsequence_row nil] (try (do (set! longest_palindromic_subsequence_rev (reverse longest_palindromic_subsequence_s)) (set! longest_palindromic_subsequence_n (count longest_palindromic_subsequence_s)) (set! longest_palindromic_subsequence_m (count longest_palindromic_subsequence_rev)) (set! longest_palindromic_subsequence_dp []) (set! longest_palindromic_subsequence_i 0) (while (<= longest_palindromic_subsequence_i longest_palindromic_subsequence_n) (do (set! longest_palindromic_subsequence_row []) (set! longest_palindromic_subsequence_j 0) (while (<= longest_palindromic_subsequence_j longest_palindromic_subsequence_m) (do (set! longest_palindromic_subsequence_row (conj longest_palindromic_subsequence_row 0)) (set! longest_palindromic_subsequence_j (+ longest_palindromic_subsequence_j 1)))) (set! longest_palindromic_subsequence_dp (conj longest_palindromic_subsequence_dp longest_palindromic_subsequence_row)) (set! longest_palindromic_subsequence_i (+ longest_palindromic_subsequence_i 1)))) (set! longest_palindromic_subsequence_i 1) (while (<= longest_palindromic_subsequence_i longest_palindromic_subsequence_n) (do (set! longest_palindromic_subsequence_j 1) (while (<= longest_palindromic_subsequence_j longest_palindromic_subsequence_m) (do (set! longest_palindromic_subsequence_a_char (subs longest_palindromic_subsequence_s (- longest_palindromic_subsequence_i 1) (min longest_palindromic_subsequence_i (count longest_palindromic_subsequence_s)))) (set! longest_palindromic_subsequence_b_char (subs longest_palindromic_subsequence_rev (- longest_palindromic_subsequence_j 1) (min longest_palindromic_subsequence_j (count longest_palindromic_subsequence_rev)))) (if (= longest_palindromic_subsequence_a_char longest_palindromic_subsequence_b_char) (set! longest_palindromic_subsequence_dp (assoc-in longest_palindromic_subsequence_dp [longest_palindromic_subsequence_i longest_palindromic_subsequence_j] (+ 1 (nth (nth longest_palindromic_subsequence_dp (- longest_palindromic_subsequence_i 1)) (- longest_palindromic_subsequence_j 1))))) (set! longest_palindromic_subsequence_dp (assoc-in longest_palindromic_subsequence_dp [longest_palindromic_subsequence_i longest_palindromic_subsequence_j] (max_int (nth (nth longest_palindromic_subsequence_dp (- longest_palindromic_subsequence_i 1)) longest_palindromic_subsequence_j) (nth (nth longest_palindromic_subsequence_dp longest_palindromic_subsequence_i) (- longest_palindromic_subsequence_j 1)))))) (set! longest_palindromic_subsequence_j (+ longest_palindromic_subsequence_j 1)))) (set! longest_palindromic_subsequence_i (+ longest_palindromic_subsequence_i 1)))) (throw (ex-info "return" {:v (nth (nth longest_palindromic_subsequence_dp longest_palindromic_subsequence_n) longest_palindromic_subsequence_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (longest_palindromic_subsequence "bbbab")))
      (println (str (longest_palindromic_subsequence "bbabcbcab")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
