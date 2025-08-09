;; Generated on 2025-08-09 10:22 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi time))
(define (_mem) (* 1024 (resource-usage-max-rss (get-resource-usage resource-usage/self))))
(import (chibi json))
(define (to-str x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str x) ", ") "]"))
        ((hash-table? x)
         (let* ((ks (hash-table-keys x))
                (pairs (map (lambda (k)
                              (string-append (to-str k) ": " (to-str (hash-table-ref x k))))
                            ks)))
           (string-append "{" (string-join pairs ", ") "}")))
        ((null? x) "[]")
        ((string? x) (let ((out (open-output-string))) (json-write x out) (get-output-string out)))
        ((boolean? x) (if x "true" "false"))
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
        (else (number->string x))))
(define (to-str-space x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str-space x) " ") "]"))
        ((string? x) x)
        (else (to-str x))))
(define (upper s) (string-upcase s))
(define (lower s) (string-downcase s))
(define _floor floor)
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
(define (_mod a b) (if (and (integer? a) (integer? b)) (modulo a b) (fmod a b)))
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
(define (_gt a b) (cond ((and (number? a) (number? b)) (> a b)) ((and (string? a) (string? b)) (string>? a b)) (else (> a b))))
(define (_lt a b) (cond ((and (number? a) (number? b)) (< a b)) ((and (string? a) (string? b)) (string<? a b)) (else (< a b))))
(define (_ge a b) (cond ((and (number? a) (number? b)) (>= a b)) ((and (string? a) (string? b)) (string>=? a b)) (else (>= a b))))
(define (_le a b) (cond ((and (number? a) (number? b)) (<= a b)) ((and (string? a) (string? b)) (string<=? a b)) (else (<= a b))))
(define (_add a b)
  (cond ((and (number? a) (number? b)) (+ a b))
        ((string? a) (string-append a (to-str b)))
        ((string? b) (string-append (to-str a) b))
        ((and (list? a) (list? b)) (append a b))
        (else (+ a b))))
(define (indexOf s sub) (let ((cur (string-contains s sub)))   (if cur (string-cursor->index s cur) -1)))
(define (_display . args) (apply display args))
(define (panic msg) (error msg))
(define (padStart s width pad)
  (let loop ((out s))
    (if (< (string-length out) width)
        (loop (string-append pad out))
        out)))
(define (_substring s start end)
  (let* ((len (string-length s))
         (s0 (max 0 (min len start)))
         (e0 (max s0 (min len end))))
    (substring s s0 e0)))
(define (_repeat s n)
  (let loop ((i 0) (out ""))
    (if (< i n)
        (loop (+ i 1) (string-append out s))
        out)))
(define (slice seq start end)
  (let* ((len (if (string? seq) (string-length seq) (length seq)))
         (s (if (< start 0) (+ len start) start))
         (e (if (< end 0) (+ len end) end)))
    (set! s (max 0 (min len s)))
    (set! e (max 0 (min len e)))
    (when (< e s) (set! e s))
    (if (string? seq)
        (_substring seq s e)
        (take (drop seq s) (- e s)))))
(define (_parseIntStr s base)
  (let* ((b (if (number? base) base 10))
         (n (string->number (if (list? s) (list->string s) s) b)))
    (if n (inexact->exact (truncate n)) 0)))
(define (_split s sep)
  (let* ((str (if (string? s) s (list->string s)))
         (del (cond ((char? sep) sep)
                     ((string? sep) (if (= (string-length sep) 1)
                                       (string-ref sep 0)
                                       sep))
                     (else sep))))
    (cond
     ((and (string? del) (string=? del ""))
      (map string (string->list str)))
     ((char? del)
      (string-split str del))
     (else
        (let loop ((r str) (acc '()))
          (let ((cur (string-contains r del)))
            (if cur
                (let ((idx (string-cursor->index r cur)))
                  (loop (_substring r (+ idx (string-length del)) (string-length r))
                        (cons (_substring r 0 idx) acc)))
                (reverse (cons r acc)))))))))
(define (_len x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-table-size x))
        (else (length x))))
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
(
  let (
    (
      start5 (
        current-jiffy
      )
    )
     (
      jps8 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          numbers (
            _list "37107287533902102798797998220837590246510135740250" "46376937677490009712648124896970078050417018260538" "74324986199524741059474233309513058123726617309629" "91942213363574161572522430563301811072406154908250" "23067588207539346171171980310421047513778063246676" "89261670696623633820136378418383684178734361726757" "28112879812849979408065481931592621691275889832738" "44274228917432520321923589422876796487670272189318" "47451445736001306439091167216856844588711603153276" "70386486105843025439939619828917593665686757934951" "62176457141856560629502157223196586755079324193331" "64906352462741904929101432445813822663347944758178" "92575867718337217661963751590579239728245598838407" "58203565325359399008402633568948830189458628227828" "80181199384826282014278194139940567587151170094390" "35398664372827112653829987240784473053190104293586" "86515506006295864861532075273371959191420517255829" "71693888707715466499115593487603532921714970056938" "54370070576826684624621495650076471787294438377604" "53282654108756828443191190634694037855217779295145" "36123272525000296071075082563815656710885258350721" "45876576172410976447339110607218265236877223636045" "17423706905851860660448207621209813287860733969412" "81142660418086830619328460811191061556940512689692" "51934325451728388641918047049293215058642563049483" "62467221648435076201727918039944693004732956340691" "15732444386908125794514089057706229429197107928209" "55037687525678773091862540744969844508330393682126" "18336384825330154686196124348767681297534375946515" "80386287592878490201521685554828717201219257766954" "78182833757993103614740356856449095527097864797581" "16726320100436897842553539920931837441497806860984" "48403098129077791799088218795327364475675590848030" "87086987551392711854517078544161852424320693150332" "59959406895756536782107074926966537676326235447210" "69793950679652694742597709739166693763042633987085" "41052684708299085211399427365734116182760315001271" "65378607361501080857009149939512557028198746004375" "35829035317434717326932123578154982629742552737307" "94953759765105305946966067683156574377167401875275" "88902802571733229619176668713819931811048770190271" "25267680276078003013678680992525463401061632866526" "36270218540497705585629946580636237993140746255962" "24074486908231174977792365466257246923322810917141" "91430288197103288597806669760892938638285025333403" "34413065578016127815921815005561868836468420090470" "23053081172816430487623791969842487255036638784583" "11487696932154902810424020138335124462181441773470" "63783299490636259666498587618221225225512486764533" "67720186971698544312419572409913959008952310058822" "95548255300263520781532296796249481641953868218774" "76085327132285723110424803456124867697064507995236" "37774242535411291684276865538926205024910326572967" "23701913275725675285653248258265463092207058596522" "29798860272258331913126375147341994889534765745501" "18495701454879288984856827726077713721403798879715" "38298203783031473527721580348144513491373226651381" "34829543829199918180278916522431027392251122869539" "40957953066405232632538044100059654939159879593635" "29746152185502371307642255121183693803580388584903" "41698116222072977186158236678424689157993532961922" "62467957194401269043877107275048102390895523597457" "23189706772547915061505504953922979530901129967519" "86188088225875314529584099251203829009407770775672" "11306739708304724483816533873502340845647058077308" "82959174767140363198008187129011875491310547126581" "97623331044818386269515456334926366572897563400500" "42846280183517070527831839425882145521227251250327" "55121603546981200581762165212827652751691296897789" "32238195734329339946437501907836945765883352399886" "75506164965184775180738168837861091527357929701337" "62177842752192623401942399639168044983993173312731" "32924185707147349566916674687634660915035914677504" "99518671430235219628894890102423325116913619626622" "73267460800591547471830798392868535206946944540724" "76841822524674417161514036427982273348055556214818" "97142617910342598647204516893989422179826088076852" "87783646182799346313767754307809363333018982642090" "10848802521674670883215120185883543223812876952786" "71329612474782464538636993009049310363619763878039" "62184073572399794223406235393808339651327408011116" "66627891981488087797941876876144230030984490851411" "60661826293682836764744779239180335110989069790714" "85786944089552990653640447425576083659976645795096" "66024396409905389607120198219976047599490197230297" "64913982680032973156037120041377903785566085089252" "16730939319872750275468906903707539413042652315011" "94809377245048795150954100921645863754710598436791" "78639167021187492431995700641917969777599028300699" "15368713711936614952811305876380278410754449733078" "40789923115535562561142322423255033685442488917353" "44889911501440648020369068063960672322193204149535" "41503128880339536053299340368006977710650566631954" "81234880673210146739058568557934581403627822703280" "82616570773948327592232845941706525094512325230608" "22918802058777319719839450180888072429661980811197" "77158542502016545090413245809786882778948721859617" "72107838435069186155435662884062257473692284509516" "20849603980134001723930671666823555245252804609722" "53503534226472524250874054075591789781264330331690"
          )
        )
      )
       (
        begin (
          define (
            add_strings a b
          )
           (
            let (
              (
                i (
                  - (
                    _len a
                  )
                   1
                )
              )
            )
             (
              begin (
                let (
                  (
                    j (
                      - (
                        _len b
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        carry 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            res ""
                          )
                        )
                         (
                          begin (
                            letrec (
                              (
                                loop1 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      or (
                                        or (
                                          >= i 0
                                        )
                                         (
                                          >= j 0
                                        )
                                      )
                                       (
                                        > carry 0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            da 0
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              >= i 0
                                            )
                                             (
                                              begin (
                                                set! da (
                                                  - (
                                                    let (
                                                      (
                                                        v2 (
                                                          _substring a i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      cond (
                                                        (
                                                          string? v2
                                                        )
                                                         (
                                                          inexact->exact (
                                                            _floor (
                                                              string->number v2
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          boolean? v2
                                                        )
                                                         (
                                                          if v2 1 0
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          inexact->exact (
                                                            _floor v2
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   48
                                                )
                                              )
                                               (
                                                set! i (
                                                  - i 1
                                                )
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                db 0
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  >= j 0
                                                )
                                                 (
                                                  begin (
                                                    set! db (
                                                      - (
                                                        let (
                                                          (
                                                            v3 (
                                                              _substring b j (
                                                                + j 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          cond (
                                                            (
                                                              string? v3
                                                            )
                                                             (
                                                              inexact->exact (
                                                                _floor (
                                                                  string->number v3
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              boolean? v3
                                                            )
                                                             (
                                                              if v3 1 0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              inexact->exact (
                                                                _floor v3
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       48
                                                    )
                                                  )
                                                   (
                                                    set! j (
                                                      - j 1
                                                    )
                                                  )
                                                )
                                                 '(
                                                  
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    s (
                                                      + (
                                                        + da db
                                                      )
                                                       carry
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! carry (
                                                      _div s 10
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        digit (
                                                          _mod s 10
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          string-append (
                                                            to-str-space digit
                                                          )
                                                           res
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop1
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop1
                            )
                          )
                           res
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            first_ten_digits nums
          )
           (
            let (
              (
                total "0"
              )
            )
             (
              begin (
                let (
                  (
                    idx 0
                  )
                )
                 (
                  begin (
                    letrec (
                      (
                        loop4 (
                          lambda (
                            
                          )
                           (
                            if (
                              < idx (
                                _len nums
                              )
                            )
                             (
                              begin (
                                set! total (
                                  add_strings total (
                                    list-ref-safe nums idx
                                  )
                                )
                              )
                               (
                                set! idx (
                                  + idx 1
                                )
                              )
                               (
                                loop4
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      loop4
                    )
                  )
                   (
                    _substring total 0 10
                  )
                )
              )
            )
          )
        )
         (
          define (
            main
          )
           (
            let (
              (
                ans (
                  first_ten_digits numbers
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? ans
                  )
                   ans (
                    to-str ans
                  )
                )
              )
               (
                newline
              )
            )
          )
        )
         (
          main
        )
      )
    )
     (
      let (
        (
          end6 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur7 (
              quotient (
                * (
                  - end6 start5
                )
                 1000000
              )
               jps8
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur7
              )
               ",\n  \"memory_bytes\": " (
                number->string (
                  _mem
                )
              )
               ",\n  \"name\": \"main\"\n}"
            )
          )
           (
            newline
          )
        )
      )
    )
  )
)
