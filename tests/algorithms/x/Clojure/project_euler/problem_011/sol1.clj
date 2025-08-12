(ns main (:refer-clojure :exclude [largest_product main]))

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

(declare largest_product main)

(def ^:dynamic largest_product_horz nil)

(def ^:dynamic largest_product_i nil)

(def ^:dynamic largest_product_j nil)

(def ^:dynamic largest_product_largest nil)

(def ^:dynamic largest_product_lr nil)

(def ^:dynamic largest_product_n_cols nil)

(def ^:dynamic largest_product_n_rows nil)

(def ^:dynamic largest_product_rl nil)

(def ^:dynamic largest_product_vert nil)

(def ^:dynamic main_ans nil)

(def ^:dynamic main_grid nil)

(defn largest_product [largest_product_grid]
  (binding [largest_product_horz nil largest_product_i nil largest_product_j nil largest_product_largest nil largest_product_lr nil largest_product_n_cols nil largest_product_n_rows nil largest_product_rl nil largest_product_vert nil] (try (do (set! largest_product_n_rows (count largest_product_grid)) (set! largest_product_n_cols (count (nth largest_product_grid 0))) (set! largest_product_largest 0) (set! largest_product_i 0) (while (< largest_product_i largest_product_n_rows) (do (set! largest_product_j 0) (while (< largest_product_j largest_product_n_cols) (do (when (<= largest_product_j (- largest_product_n_cols 4)) (do (set! largest_product_horz (* (* (* (nth (nth largest_product_grid largest_product_i) largest_product_j) (nth (nth largest_product_grid largest_product_i) (+ largest_product_j 1))) (nth (nth largest_product_grid largest_product_i) (+ largest_product_j 2))) (nth (nth largest_product_grid largest_product_i) (+ largest_product_j 3)))) (when (> largest_product_horz largest_product_largest) (set! largest_product_largest largest_product_horz)))) (when (<= largest_product_i (- largest_product_n_rows 4)) (do (set! largest_product_vert (* (* (* (nth (nth largest_product_grid largest_product_i) largest_product_j) (nth (nth largest_product_grid (+ largest_product_i 1)) largest_product_j)) (nth (nth largest_product_grid (+ largest_product_i 2)) largest_product_j)) (nth (nth largest_product_grid (+ largest_product_i 3)) largest_product_j))) (when (> largest_product_vert largest_product_largest) (set! largest_product_largest largest_product_vert)) (when (<= largest_product_j (- largest_product_n_cols 4)) (do (set! largest_product_lr (* (* (* (nth (nth largest_product_grid largest_product_i) largest_product_j) (nth (nth largest_product_grid (+ largest_product_i 1)) (+ largest_product_j 1))) (nth (nth largest_product_grid (+ largest_product_i 2)) (+ largest_product_j 2))) (nth (nth largest_product_grid (+ largest_product_i 3)) (+ largest_product_j 3)))) (when (> largest_product_lr largest_product_largest) (set! largest_product_largest largest_product_lr)))) (when (>= largest_product_j 3) (do (set! largest_product_rl (* (* (* (nth (nth largest_product_grid largest_product_i) largest_product_j) (nth (nth largest_product_grid (+ largest_product_i 1)) (- largest_product_j 1))) (nth (nth largest_product_grid (+ largest_product_i 2)) (- largest_product_j 2))) (nth (nth largest_product_grid (+ largest_product_i 3)) (- largest_product_j 3)))) (when (> largest_product_rl largest_product_largest) (set! largest_product_largest largest_product_rl)))))) (set! largest_product_j (+ largest_product_j 1)))) (set! largest_product_i (+ largest_product_i 1)))) (throw (ex-info "return" {:v largest_product_largest}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ans nil main_grid nil] (do (set! main_grid [[8 2 22 97 38 15 0 40 0 75 4 5 7 78 52 12 50 77 91 8] [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 4 56 62 0] [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 3 49 13 36 65] [52 70 95 23 4 60 11 42 69 24 68 56 1 32 56 71 37 2 36 91] [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80] [24 47 32 60 99 3 45 2 44 75 33 53 78 36 84 20 35 17 12 50] [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70] [67 26 20 68 2 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21] [24 55 58 5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72] [21 36 23 9 75 0 76 44 20 45 35 14 0 61 33 97 34 31 33 95] [78 17 53 28 22 75 31 67 15 94 3 80 4 62 16 14 9 53 56 92] [16 39 5 42 96 35 31 47 55 58 88 24 0 17 54 24 36 29 85 57] [86 56 0 48 35 71 89 7 5 44 44 37 44 60 21 58 51 54 17 58] [19 80 81 68 5 94 47 69 28 73 92 13 86 52 17 77 4 89 55 40] [4 52 8 83 97 35 99 16 7 97 57 32 16 26 26 79 33 27 98 66] [88 36 68 87 57 62 20 72 3 46 33 67 46 55 12 32 63 93 53 69] [4 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36] [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 4 36 16] [20 73 35 29 78 31 90 1 74 31 49 71 48 86 81 16 23 57 5 54] [1 70 54 71 83 51 54 69 16 92 33 48 61 43 52 1 89 19 67 48]]) (set! main_ans (largest_product main_grid)) (println (str main_ans)))))

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
