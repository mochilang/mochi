(ns main (:refer-clojure :exclude [capitalize say fourIsMagic main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare capitalize say fourIsMagic main)

(declare i illions ix nn nums p s small sx t tens)

(defn capitalize [s]
  (try (if (= (count s) 0) s (str (clojure.string/upper-case (subs s 0 1)) (subs s 1 (count s)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def small ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(def tens ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(def illions ["" " thousand" " million" " billion" " trillion" " quadrillion" " quintillion"])

(defn say [n_p]
  (try (do (def n n_p) (def t "") (when (< n 0) (do (def t "negative ") (def n (- n)))) (if (< n 20) (throw (ex-info "return" {:v (str t (nth small n))})) (if (< n 100) (do (def t (nth tens (/ n 10))) (def s (mod n 10)) (when (> s 0) (def t (str (str t "-") (nth small s)))) (throw (ex-info "return" {:v t}))) (when (< n 1000) (do (def t (str (nth small (/ n 100)) " hundred")) (def s (mod n 100)) (when (> s 0) (def t (str (str t " ") (say s)))) (throw (ex-info "return" {:v t})))))) (def sx "") (def i 0) (def nn n) (while (> nn 0) (do (def p (mod nn 1000)) (def nn (/ nn 1000)) (when (> p 0) (do (def ix (str (say p) (nth illions i))) (when (not= sx "") (def ix (str (str ix " ") sx))) (def sx ix))) (def i (+' i 1)))) (throw (ex-info "return" {:v (str t sx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fourIsMagic [n_p]
  (try (do (def n n_p) (def s (say n)) (def s (capitalize s)) (def t s) (while (not= n 4) (do (def n (count s)) (def s (say n)) (def t (str (str (str (str t " is ") s) ", ") s)))) (def t (str t " is magic.")) (throw (ex-info "return" {:v t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def nums [0 4 6 11 13 75 100 337 (- 164) 9223372036854775807]) (doseq [n nums] (println (fourIsMagic n)))))

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
