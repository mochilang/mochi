(ns main (:refer-clojure :exclude [str_eval solution main]))

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

(declare str_eval solution main)

(def ^:dynamic main_res nil)

(def ^:dynamic solution_cur_index nil)

(def ^:dynamic solution_largest_product nil)

(def ^:dynamic solution_prod nil)

(def ^:dynamic solution_substr nil)

(def ^:dynamic str_eval_i nil)

(def ^:dynamic str_eval_product nil)

(def ^:dynamic main_N (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str "73167176531330624919225119674426574742355349194934" "96983520312774506326239578318016984801869478851843") "85861560789112949495459501737958331952853208805511") "12540698747158523863050715693290963295227443043557") "66896648950445244523161731856403098711121722383113") "62229893423380308135336276614282806444486645238749") "30358907296290491560440772390713810515859307960866") "70172427121883998797908792274921901699720888093776") "65727333001053367881220235421809751254540594752243") "52584907711670556013604839586446706324415722155397") "53697817977846174064955149290862569321978468622482") "83972241375657056057490261407972968652414535100474") "82166370484403199890008895243450658541227588666881") "16427171479924442928230863465674813919123162824586") "17866458359124566529476545682848912883142607690042") "24219022671055626321111109370544217506941658960408") "07198403850962455444362981230987879927244284909188") "84580156166097919133875499200524063689912560717606") "05886116467109405077541002256983155200055935729725") "71636269561882670428252483600823257530420752963450"))

(defn str_eval [str_eval_s]
  (binding [str_eval_i nil str_eval_product nil] (try (do (set! str_eval_product 1) (set! str_eval_i 0) (while (< str_eval_i (count str_eval_s)) (do (set! str_eval_product (* str_eval_product (Long/parseLong (subs str_eval_s str_eval_i (min (+ str_eval_i 1) (count str_eval_s)))))) (set! str_eval_i (+ str_eval_i 1)))) (throw (ex-info "return" {:v str_eval_product}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_cur_index nil solution_largest_product nil solution_prod nil solution_substr nil] (try (do (set! solution_largest_product (- 1)) (set! solution_substr (subs solution_n 0 (min 13 (count solution_n)))) (set! solution_cur_index 13) (while (< solution_cur_index (- (count solution_n) 13)) (if (>= (Long/parseLong (subs solution_n solution_cur_index (min (+ solution_cur_index 1) (count solution_n)))) (Long/parseLong (subs solution_substr 0 (min 1 (count solution_substr))))) (do (set! solution_substr (str (subs solution_substr 1 (min (count solution_substr) (count solution_substr))) (subs solution_n solution_cur_index (min (+ solution_cur_index 1) (count solution_n))))) (set! solution_cur_index (+ solution_cur_index 1))) (do (set! solution_prod (str_eval solution_substr)) (when (> solution_prod solution_largest_product) (set! solution_largest_product solution_prod)) (set! solution_substr (subs solution_n solution_cur_index (min (+ solution_cur_index 13) (count solution_n)))) (set! solution_cur_index (+ solution_cur_index 13))))) (throw (ex-info "return" {:v solution_largest_product}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_res nil] (do (set! main_res (solution main_N)) (println (str "solution() = " (str main_res))))))

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
