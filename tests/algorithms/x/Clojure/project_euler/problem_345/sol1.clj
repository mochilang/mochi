(ns main (:refer-clojure :exclude [parse_row parse_matrix bitcount build_powers solution]))

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

(declare parse_row parse_matrix bitcount build_powers solution)

(declare _read_file)

(def ^:dynamic bitcount_y nil)

(def ^:dynamic build_powers_current nil)

(def ^:dynamic build_powers_i nil)

(def ^:dynamic build_powers_powers nil)

(def ^:dynamic count_v nil)

(def ^:dynamic parse_matrix_matrix nil)

(def ^:dynamic parse_matrix_row nil)

(def ^:dynamic parse_matrix_row_str nil)

(def ^:dynamic parse_row_ch nil)

(def ^:dynamic parse_row_current nil)

(def ^:dynamic parse_row_has_digit nil)

(def ^:dynamic parse_row_i nil)

(def ^:dynamic parse_row_nums nil)

(def ^:dynamic solution_arr nil)

(def ^:dynamic solution_col nil)

(def ^:dynamic solution_dp nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_mask nil)

(def ^:dynamic solution_n nil)

(def ^:dynamic solution_new_mask nil)

(def ^:dynamic solution_powers nil)

(def ^:dynamic solution_row nil)

(def ^:dynamic solution_size nil)

(def ^:dynamic solution_value nil)

(defn parse_row [parse_row_row_str]
  (binding [parse_row_ch nil parse_row_current nil parse_row_has_digit nil parse_row_i nil parse_row_nums nil] (try (do (set! parse_row_nums []) (set! parse_row_current 0) (set! parse_row_has_digit false) (set! parse_row_i 0) (while (< parse_row_i (count parse_row_row_str)) (do (set! parse_row_ch (subs parse_row_row_str parse_row_i (min (+ parse_row_i 1) (count parse_row_row_str)))) (if (= parse_row_ch " ") (when parse_row_has_digit (do (set! parse_row_nums (conj parse_row_nums parse_row_current)) (set! parse_row_current 0) (set! parse_row_has_digit false))) (do (set! parse_row_current (+ (* parse_row_current 10) (Long/parseLong parse_row_ch))) (set! parse_row_has_digit true))) (set! parse_row_i (+ parse_row_i 1)))) (when parse_row_has_digit (set! parse_row_nums (conj parse_row_nums parse_row_current))) (throw (ex-info "return" {:v parse_row_nums}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_matrix [parse_matrix_matrix_str]
  (binding [parse_matrix_matrix nil parse_matrix_row nil parse_matrix_row_str nil] (try (do (set! parse_matrix_matrix []) (doseq [parse_matrix_row_str parse_matrix_matrix_str] (do (set! parse_matrix_row (parse_row parse_matrix_row_str)) (set! parse_matrix_matrix (conj parse_matrix_matrix parse_matrix_row)))) (throw (ex-info "return" {:v parse_matrix_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bitcount [bitcount_x]
  (binding [bitcount_y nil count_v nil] (try (do (set! count_v 0) (set! bitcount_y bitcount_x) (while (> bitcount_y 0) (do (when (= (mod bitcount_y 2) 1) (set! count_v (+ count_v 1))) (set! bitcount_y (/ bitcount_y 2)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_powers [build_powers_n]
  (binding [build_powers_current nil build_powers_i nil build_powers_powers nil] (try (do (set! build_powers_powers []) (set! build_powers_i 0) (set! build_powers_current 1) (while (<= build_powers_i build_powers_n) (do (set! build_powers_powers (conj build_powers_powers build_powers_current)) (set! build_powers_current (* build_powers_current 2)) (set! build_powers_i (+ build_powers_i 1)))) (throw (ex-info "return" {:v build_powers_powers}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_matrix_str]
  (binding [solution_arr nil solution_col nil solution_dp nil solution_i nil solution_mask nil solution_n nil solution_new_mask nil solution_powers nil solution_row nil solution_size nil solution_value nil] (try (do (set! solution_arr (parse_matrix solution_matrix_str)) (set! solution_n (count solution_arr)) (set! solution_powers (build_powers solution_n)) (set! solution_size (nth solution_powers solution_n)) (set! solution_dp []) (set! solution_i 0) (while (< solution_i solution_size) (do (set! solution_dp (conj solution_dp 0)) (set! solution_i (+ solution_i 1)))) (set! solution_mask 0) (while (< solution_mask solution_size) (do (set! solution_row (bitcount solution_mask)) (when (< solution_row solution_n) (do (set! solution_col 0) (while (< solution_col solution_n) (do (when (= (mod (/ solution_mask (nth solution_powers solution_col)) 2) 0) (do (set! solution_new_mask (+ solution_mask (nth solution_powers solution_col))) (set! solution_value (+ (nth solution_dp solution_mask) (nth (nth solution_arr solution_row) solution_col))) (when (> solution_value (nth solution_dp solution_new_mask)) (set! solution_dp (assoc solution_dp solution_new_mask solution_value))))) (set! solution_col (+ solution_col 1)))))) (set! solution_mask (+ solution_mask 1)))) (throw (ex-info "return" {:v (nth solution_dp (- solution_size 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_MATRIX_2 nil)

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_MATRIX_2) (constantly ["7 53 183 439 863 497 383 563 79 973 287 63 343 169 583" "627 343 773 959 943 767 473 103 699 303 957 703 583 639 913" "447 283 463 29 23 487 463 993 119 883 327 493 423 159 743" "217 623 3 399 853 407 103 983 89 463 290 516 212 462 350" "960 376 682 962 300 780 486 502 912 800 250 346 172 812 350" "870 456 192 162 593 473 915 45 989 873 823 965 425 329 803" "973 965 905 919 133 673 665 235 509 613 673 815 165 992 326" "322 148 972 962 286 255 941 541 265 323 925 281 601 95 973" "445 721 11 525 473 65 511 164 138 672 18 428 154 448 848" "414 456 310 312 798 104 566 520 302 248 694 976 430 392 198" "184 829 373 181 631 101 969 613 840 740 778 458 284 760 390" "821 461 843 513 17 901 711 993 293 157 274 94 192 156 574" "34 124 4 878 450 476 712 914 838 669 875 299 823 329 699" "815 559 813 459 522 788 168 586 966 232 308 833 251 631 107" "813 883 451 509 615 77 281 613 459 205 380 274 302 35 805"]))
      (alter-var-root (var main_result) (constantly (solution main_MATRIX_2)))
      (println (str "solution() = " (mochi_str main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
