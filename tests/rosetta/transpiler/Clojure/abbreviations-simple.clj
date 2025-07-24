(ns main (:refer-clojure :exclude [fields padRight join parseIntStr isDigits readTable validate main]))

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(defn fields [s]
  (try (do (def words []) (def cur "") (def i 0) (while (< i (count s)) (do (def ch (subs s i (+ i 1))) (if (or (or (= ch " ") (= ch "\n")) (= ch "\t")) (when (> (count cur) 0) (do (def words (conj words cur)) (def cur ""))) (def cur (str cur ch))) (def i (+ i 1)))) (when (> (count cur) 0) (def words (conj words cur))) (throw (ex-info "return" {:v words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padRight [s width]
  (try (do (def out s) (def i (count s)) (while (< i width) (do (def out (str out " ")) (def i (+ i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn join [xs sep]
  (try (do (def res "") (def i 0) (while (< i (count xs)) (do (when (> i 0) (def res (str res sep))) (def res (str res (nth xs i))) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseIntStr [str]
  (try (do (def i 0) (def neg false) (when (and (> (count str) 0) (= (subs str 0 1) "-")) (do (def neg true) (def i 1))) (def n 0) (def digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< i (count str)) (do (def n (+ (* n 10) (nth digits (subs str i (+ i 1))))) (def i (+ i 1)))) (when neg (def n (- n))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isDigits [s]
  (try (do (when (= (count s) 0) (throw (ex-info "return" {:v false}))) (def i 0) (while (< i (count s)) (do (def ch (subs s i (+ i 1))) (when (or (< ch "0") (> ch "9")) (throw (ex-info "return" {:v false}))) (def i (+ i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn readTable [table]
  (try (do (def toks (fields table)) (def cmds []) (def mins []) (def i 0) (while (< i (count toks)) (do (def cmd (nth toks i)) (def minlen (count cmd)) (def i (+ i 1)) (when (and (< i (count toks)) (isDigits (nth toks i))) (do (def num (parseIntStr (nth toks i))) (when (and (>= num 1) (< num (count cmd))) (do (def minlen num) (def i (+ i 1)))))) (def cmds (conj cmds cmd)) (def mins (conj mins minlen)))) (throw (ex-info "return" {:v {"commands" cmds "mins" mins}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn validate [commands mins words]
  (try (do (def results []) (def wi 0) (while (< wi (count words)) (do (def w (nth words wi)) (def found false) (def wlen (count w)) (def ci 0) (loop [while_flag_1 true] (when (and while_flag_1 (< ci (count commands))) (do (def cmd (nth commands ci)) (cond (and (and (and (not= (nth mins ci) 0) (>= wlen (nth mins ci))) (<= wlen (count cmd))) (= (subs c 0 wlen) ww)) (do (def c (upper cmd)) (def ww (upper w)) (do (def results (conj results c)) (def found true) (recur false))) :else (do (def ci (+ ci 1)) (recur while_flag_1)))))) (when (not found) (def results (conj results "*error*"))) (def wi (+ wi 1)))) (throw (ex-info "return" {:v results}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def table (str (str (str (str (str (str (str (str "" "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ") "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ") "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ") "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ") "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ") "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ") "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ") "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 ")) (def sentence "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin") (def tbl (readTable table)) (def commands (get tbl "commands")) (def mins (get tbl "mins")) (def words (fields sentence)) (def results (validate commands mins words)) (def out1 "user words:") (def k 0) (while (< k (count words)) (do (def out1 (str out1 " ")) (if (< k (- (count words) 1)) (def out1 (str out1 (padRight (nth words k) (count (nth results k))))) (def out1 (str out1 (nth words k)))) (def k (+ k 1)))) (println out1) (println (str "full words: " (join results " ")))))

(defn -main []
  (main))

(-main)
