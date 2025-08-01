(ns main (:refer-clojure :exclude [say sayOrdinal split countLetters wordLen totalLength pad main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare say sayOrdinal split countLetters wordLen totalLength pad main)

(declare ch cnt cur i idx j l line m n parts r res s small smallOrd tens tensOrd tot w word words)

(def small ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(def tens ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(def smallOrd ["zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth" "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"])

(def tensOrd ["" "" "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"])

(defn say [n]
  (try (do (when (< n 20) (throw (ex-info "return" {:v (nth small n)}))) (when (< n 100) (do (def res (nth tens (/ n 10))) (def m (mod n 10)) (when (not= m 0) (def res (str (str res "-") (nth small m)))) (throw (ex-info "return" {:v res})))) (when (< n 1000) (do (def res (str (say (/ n 100)) " hundred")) (def m (mod n 100)) (when (not= m 0) (def res (str (str res " ") (say m)))) (throw (ex-info "return" {:v res})))) (when (< n 1000000) (do (def res (str (say (/ n 1000)) " thousand")) (def m (mod n 1000)) (when (not= m 0) (def res (str (str res " ") (say m)))) (throw (ex-info "return" {:v res})))) (def res (str (say (/ n 1000000)) " million")) (def m (mod n 1000000)) (when (not= m 0) (def res (str (str res " ") (say m)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sayOrdinal [n]
  (try (do (when (< n 20) (throw (ex-info "return" {:v (nth smallOrd n)}))) (when (< n 100) (do (when (= (mod n 10) 0) (throw (ex-info "return" {:v (nth tensOrd (/ n 10))}))) (throw (ex-info "return" {:v (str (str (say (- n (mod n 10))) "-") (nth smallOrd (mod n 10)))})))) (when (< n 1000) (do (when (= (mod n 100) 0) (throw (ex-info "return" {:v (str (say (/ n 100)) " hundredth")}))) (throw (ex-info "return" {:v (str (str (say (/ n 100)) " hundred ") (sayOrdinal (mod n 100)))})))) (when (< n 1000000) (do (when (= (mod n 1000) 0) (throw (ex-info "return" {:v (str (say (/ n 1000)) " thousandth")}))) (throw (ex-info "return" {:v (str (str (say (/ n 1000)) " thousand ") (sayOrdinal (mod n 1000)))})))) (if (= (mod n 1000000) 0) (str (say (/ n 1000000)) " millionth") (str (str (say (/ n 1000000)) " million ") (sayOrdinal (mod n 1000000))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn split [s sep]
  (try (do (def parts []) (def cur "") (def i 0) (while (< i (count s)) (if (and (and (> (count sep) 0) (<= (+' i (count sep)) (count s))) (= (subs s i (+' i (count sep))) sep)) (do (def parts (conj parts cur)) (def cur "") (def i (+' i (count sep)))) (do (def cur (str cur (subs s i (+' i 1)))) (def i (+' i 1))))) (def parts (conj parts cur)) (throw (ex-info "return" {:v parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn countLetters [s]
  (try (do (def cnt 0) (def i 0) (while (< i (count s)) (do (def ch (subs s i (+' i 1))) (when (or (and (>= (compare ch "A") 0) (<= (compare ch "Z") 0)) (and (>= (compare ch "a") 0) (<= (compare ch "z") 0))) (def cnt (+' cnt 1))) (def i (+' i 1)))) (throw (ex-info "return" {:v cnt}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def words ["Four" "is" "the" "number" "of" "letters" "in" "the" "first" "word" "of" "this" "sentence,"])

(def idx 0)

(defn wordLen [w]
  (try (do (while (< (count words) w) (do (def idx (+' idx 1)) (def n (countLetters (nth words idx))) (def parts (split (say n) " ")) (def j 0) (while (< j (count parts)) (do (def words (conj words (nth parts j))) (def j (+' j 1)))) (def words (conj words "in")) (def words (conj words "the")) (def parts (split (str (sayOrdinal (+' idx 1)) ",") " ")) (def j 0) (while (< j (count parts)) (do (def words (conj words (nth parts j))) (def j (+' j 1)))))) (def word (nth words (- w 1))) (throw (ex-info "return" {:v [word (countLetters word)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn totalLength []
  (try (do (def tot 0) (def i 0) (while (< i (count words)) (do (def tot (+' tot (count (nth words i)))) (when (< i (- (count words) 1)) (def tot (+' tot 1))) (def i (+' i 1)))) (throw (ex-info "return" {:v tot}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n width]
  (try (do (def s (str n)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println "The lengths of the first 201 words are:") (def line "") (def i 1) (while (<= i 201) (do (when (= (mod i 25) 1) (do (when (not= i 1) (println line)) (def line (str (pad i 3) ":")))) (def r (wordLen i)) (def n (nth r 1)) (def line (str (str line " ") (pad n 2))) (def i (+' i 1)))) (println line) (println (str "Length of sentence so far: " (str (totalLength)))) (doseq [n [1000 10000 100000 1000000 10000000]] (do (def r (wordLen n)) (def w (nth r 0)) (def l (nth r 1)) (println (str (str (str (str (str (str (str "Word " (pad n 8)) " is \"") w) "\", with ") (str l)) " letters.  Length of sentence so far: ") (str (totalLength))))))))

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
