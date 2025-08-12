(ns main (:refer-clojure :exclude [solution]))

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

(declare solution)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_largest_product nil)

(def ^:dynamic solution_product nil)

(def ^:dynamic main_N (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str "73167176531330624919225119674426574742355349194934" "96983520312774506326239578318016984801869478851843") "85861560789112949495459501737958331952853208805511") "12540698747158523863050715693290963295227443043557") "66896648950445244523161731856403098711121722383113") "62229893423380308135336276614282806444486645238749") "30358907296290491560440772390713810515859307960866") "70172427121883998797908792274921901699720888093776") "65727333001053367881220235421809751254540594752243") "52584907711670556013604839586446706324415722155397") "53697817977846174064955149290862569321978468622482") "83972241375657056057490261407972968652414535100474") "82166370484403199890008895243450658541227588666881") "16427171479924442928230863465674813919123162824586") "17866458359124566529476545682848912883142607690042") "24219022671055626321111109370544217506941658960408") "07198403850962455444362981230987879927244284909188") "84580156166097919133875499200524063689912560717606") "05886116467109405077541002256983155200055935729725") "71636269561882670428252483600823257530420752963450"))

(defn solution [solution_n]
  (binding [solution_i nil solution_j nil solution_largest_product nil solution_product nil] (try (do (set! solution_largest_product 0) (set! solution_i 0) (while (<= solution_i (- (count solution_n) 13)) (do (set! solution_product 1) (set! solution_j 0) (while (< solution_j 13) (do (set! solution_product (* solution_product (toi (subs solution_n (+ solution_i solution_j) (min (+ (+ solution_i solution_j) 1) (count solution_n)))))) (set! solution_j (+ solution_j 1)))) (when (> solution_product solution_largest_product) (set! solution_largest_product solution_product)) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_largest_product}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution() = " (str (solution main_N))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
