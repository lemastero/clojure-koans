;; clojre features: https://github.com/clojure/clojure/blob/master/changes.md
;; 1.8
;;  - string functions
;;  - clojure.server
;; 1.7
;;  - transducers
;;  - run!
;; 1.6
;;  - some, if-some, when-some
;;  - not alpha any more:
;;  Watches - add-watch, remove-watch
;;  Transients - transient, persistent!, conj!, assoc!, dissoc!, pop!, disj!
;;  Exception data - ex-info, ex-data
;;  Promises - promise, deliver
;;  Records - defrecord
;;  Types - deftype
;;  Pretty-print tables - print-table
;; 1.5
;; - hints for arguments
;; - threading macros
;; - clojure.edn: https://github.com/edn-format/edn
;; - reducers: http://clojure.com/blog/2012/05/08/reducers-a-library-and-model-for-collection-processing.html


(ns clojure_notes)
(ns my_excercise
  (:require [clojure.string :as string]))

(defn hello []
  (println "Hello world" 42))

(doc subvec) ;; print help for given function

(= :foo (keyword 'foo) (keyword "foo") (keyword :foo))
(= 'foo (symbol "foo") (symbol 'foo))
;; (symbol :foo) ClassCastException clojure.lang.Keyword cannot be cast to java.lang.String  clojure.core/symbol (core.clj:568)

(= "No dice!"
   (try
     (pop '())
   (catch IllegalStateException e
     "No dice!")))

(= (dec 42) 41)
(= (inc 42) 43)
(not= 1 2)

(= (list 1 2 3)
   (quote(1 2 3))
   '(1 2 3)
   `(1 2 3)) ;; differen quote operator

(=
  (list 1 5)
  `(1 ~(+ 2 3)) ;; unqote operator
  '(1 5))

(with-out-str (println "this should return as a string")) ;; return string instead of printing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (str "foo" "bar") "foobar")
(= \f (get "foo" 0))
(= 3 (count "foo"))
(= "oo" (subs "foo" 1 3) (subs "foo" 1))
(= true (char? \c))
(= true (string? "foo"))
(= \c (last "abc"))

;; (ns ...
;;  (:require [clojure.string :as string] ))
;; s str string strings cs cstr strs  cljstr

(= "12" (string/join '(1 2)))
(= "1, 2" (string/join ", " '(1 2)))
(= ["1" "2"] (string/split-lines "1\n2\n\n"))
(= "oof" (string/reverse "foo"))
(= 0 (string/index-of "foo" "fo"))
(= nil (string/index-of "no" "yes"))
(= 2 (string/last-index-of "foo" "o"))
(= "foo" (string/trim " foo \t\n"))
(= true (string/blank? " \n\t"))

(format "%.3f my friend %s" 2.0 "Bob") ;; "2,000 my friend Bob"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= '(1 2) (quote (1 2)) (list 1 2))
(= 1 (first '(1 2 3)))
(= '(2 3) (rest '(1 2 3)))
(= '() (rest '(1)))
(= '() (rest '()))
(= :c (last '(:a :b :c)))
(= 3 (count '(1 2 3)))
(= '(:a :b :c) (cons :a '(:b :c)))
(= '(:a :b :c) (conj '(:b :c) :a))
(= :a (peek '(:a :b)))
(= '(:b :c) (pop '(:a :b :c)))
;; (pop '()) IllegalStateException Can't pop empty list  clojure.lang.PersistentList$EmptyList.pop (PersistentList.java:209)
;; (assoc '(1 2) 3 3) ClassCastException clojure.lang.PersistentList cannot be cast to clojure.lang.Associative  clojure.lang.RT.assoc (RT.java:792)
;; (dissoc '(1 2) 1) ClassCastException clojure.lang.PersistentList cannot be cast to clojure.lang.IPersistentMap  clojure.lang.RT.dissoc (RT.java:848)
(= nil (get '(1 2) 1)) ;; always returns nil
(list? '(1 2))
(empty? '())

(=
 (concat '(1 2) '(3 4))
 (concat '(1 2) [3 4])
 (1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= [1 2] (vec '(1 2)))
(= []  (vec  nil))
(= [nil nil] (vector nil nil))
(= :a (first [:a :b]))
(= :c (last [:a :b :c]))
(= :b (nth [:a :b :c] 1))
(= :b (get [:a :b :c] 1))
(= nil (get [:a :b :c] 42))
(= [:a :b] (pop [:a :b :c]))
;; (pop []) IllegalStateException Can't pop empty vector  clojure.lang.PersistentVector.pop (PersistentVector.java:468)
(= :b (peek [:a :b]))
(= [1 2 "foo"] (conj [1 2] "foo"))
(= [1 "foo"] (assoc [1 2] 1 "foo"))
(= [1 2 "foo"] (assoc [1 2] 2 "foo"))
;; (assoc [1 2] 3 "foo") IndexOutOfBoundsException   clojure.lang.PersistentVector.assocN (PersistentVector.java:188)
;; (dissoc [1 2 3] 2) ClassCastException clojure.lang.PersistentVector cannot be cast to clojure.lang.IPersistentMap  clojure.lang.RT.dissoc (RT.java:848)
(= [:a :c] (subvec [:a :b :c :d] 1 3)) ;; [inclusive, exclusive)
(= 1 (count [42]))
(vector? [1 2])
(empty? [])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  (:require [clojure.set :as set] ))
;; clj-set cs cset sets clj.set

(= #{1 2} (set '(1 1 2 2 2)))
(= "foo" (get #{1 "foo" 2} "foo") )
(= nil (get #{1 :foo} :bar) )
(= #{1} (disj #{1 :foo} :foo) )
(empty? #{})
;; #{1 2 3 3} IllegalArgumentException Duplicate key: 3  clojure.lang.PersistentHashSet.createWithCheck (PersistentHashSet.java:68)

(= #{1 2 3} (set/union #{1 2} #{2 3}))
(= #{2 3} (set/intersection #{1 2 3 4} #{2 3 5}))
(= #{1 4} (set/difference #{1 2 4} #{2}))

;; tree set
(def s (sorted-set 42 1 -10))
(first s) ;; -10
(rest s) ;; #{1 ,42}

(sorted-set-by com 42 -10 1) ;; provide custom comparator #{42 1 -10}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= {:a 1 :b 2} (hash-map :a 1 :b 2))
(= 2 (count {:a 1 :b 2}))
(= true (contains? {:a nil :b 2} :a))

(= 1 (get {:a 1 :b 2} :a))
(= :not-found (get {:a 1 :b 2} :c :not-found))
(= nil (get {:a 1 :b 2} :c))

(= 1 ({:a 1 :b 2} :a)) ;; use map for lookup
(= 1 (:a {:a 1 :b 2})) ;; use keyword for lookup

(= {:bar 6, :foo 42}
   (assoc {:bar 6, :foo 8} :foo 42))
(= {:bar 6, :foo 42}
   (conj {:bar 6, :foo 8} [:foo 42]))
(= {:bar 6, :foo 8 :baz 42}
   (conj {:bar 6, :foo 8} [:baz 42]))

(= {:a 1} (dissoc {:a 1 :b 2} :b))
(= {:a 1 :b 2} (dissoc {:a 1 :b 2} :c))
;; (pop {:a 1 :b 2}) ClassCastException clojure.lang.PersistentArrayMap cannot be cast to clojure.lang.IPersistentStack  clojure.lang.RT.pop (RT.java:730)
;; (peek {:a 1}) ClassCastException clojure.lang.PersistentArrayMap cannot be cast to clojure.lang.IPersistentStack  clojure.lang.RT.peek (RT.java:724)
(= {:a 1 :b 42 :c 3}
   (merge {:a 1 :b 2} {:b 42 :c 3})) ;; merge override if neccessary

(map? {1 2})
(empty? {})

(def p {:name "John" :age 42})
(update p :age inc)
(update p :age + 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (seq [1 2 3])
   (seq '(1 2 3))
   '(1 2 3))

(= (seq {:foo 1 :bar 2})
   '([:foo 1] [:bar 2]))

(= (keys {:foo 1 :bar 2})
   '(:foo :bar))
(= (vals {:foo 1 :bar 2})
   '(1 2))

(= [:foo 1]
   (first {:foo 1 :bar 2})
   (first (seq {:foo 1 :bar 2})))

(seq? '(1 2))
(not (seq? [1 2]))

(= '[2 3] (rest [1 2 3]))
(= '() (rest nil))

(= '[2 3] (next [1 2 3]))
(= nil (next nil))

(= 2
   (fnext [1 2 3])
   (second [1 2 3])
   (first (next [1 2 3])) )

(ffirst [[1 2] [3 4]]) ;; 1
(first [[1 2] [3 4]])  ;; [1 2]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nested associative collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def jdoe {:name "John" :lastname "Doe" :address {:street "Long" :city "NY"}})
(get-in jdoe [:address :street]) ;; "Long"

(def pals [{:name "John" :age 42} {:name :Frank :age 12}])
(assoc-in  pals [0 :age] 70)  ;; [{:name "John", :age 70} {:name :Frank, :age 12}]
(update-in pals [1 :age] inc) ;; [{:name "John", :age 42} {:name :Frank, :age 13}]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square [n] (* n n))   ;; function definition and assigning name (def + fn)
(= 4 (square 2) )           ;; function call
(= 4 ( (fn [n] (* n n)) 2)) ;; inline function
(= 4 ( #(* % %)         2)) ;; ... with syntactic sugar
(= 9 ( #(+ %1 %2) 4 5))     ;; ... with multiple arguments

(fn? #(+ 1))
(fn? sort)

;; declare
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defn foo [] (do-magic))
;; CompilerException java.lang.RuntimeException: Unable to resolve symbol: do-magic in this context

(declare do-magic)
(defn foo [] (do-magic))

(defn do-magic [] (println "casting powerfull spell"))
(foo)


;; let, loop, do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [not-a-symbol? (complement symbol?)]
                  (map not-a-symbol? [:a 'b "c"]))

;; print first 10 numbers
(loop [x 10]
  (when (>= x 1)
    (println x)
    (recur (dec x))))

(= 42
   (do
    (println 1)
    (println 2)
    42))

;; doseq
;; execute body for each binding (probably for side effects), returns nil
(doseq [ sign [-1 1]
         nums [1 2 3] ]
  (print (* sign nums)))
;; -1 -2 -3 1 2 3 nil

;; dynamically binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare ^:dynamic t)

(defn add-t []
  (+ t 10) )

;; (let [t 1] (add-t))
;; ClassCastException clojure.lang.Var$Unbound cannot be cast to java.lang.Number  clojure.lang.Numbers.add (Numbers.java:128)

(binding [t 1]
  (add-t)) ;; 11

;; http://stackoverflow.com/questions/1523240/let-vs-binding-in-clojure

;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ns test)
(defn- foo [] "World")
(foo) ;; "World"

(ns other)
(test/foo) ;; CompilerException java.lang.IllegalStateException: var: #'test/foo is not public,


;; HOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn multi10 [x] (* 10 x))
(defn lessThan3 [x] (<= x 3))
(def nums [1 2 3 4 5 6 7 8])

(= [10 20 30] (map multi10 [1 2 3]))
(= [1 2 3] (filter lessThan3 nums))
(= [10 20 30] (map multi10 (filter lessThan3 nums)))

(defn sum-doubles [acc v] (+ acc v))

(= 24 (reduce * [1 2 3 4]))
(= 110 (reduce sum-doubles 100 [1 2 3 4]))

(= 25
   (let [inc-and-square (comp square inc)]
     (inc-and-square 4)))

(= 99
   (let [square-and-dec (comp dec square)]
     (square-and-dec 10)))

;; partial functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def multiply5 (partial * 5))
(multiply5 4) ;; 20

;; multiple bodis and argument list for functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def foo
  (fn
    ([a b c]
     (+ a b c))
    ([a b]
     (- a b))))

(foo 1 2 3) ;; return 6
(foo 3 5) ;; returns -2

;; tail call elimination
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-even-bigint? [n]
  (loop [n   n
         acc true]
    (if (= n 0)
      acc
      (recur (dec n) (not acc)))))

(defn recursive-reverse [coll]
  (loop [soFar []
         restOfList (vec coll)]
    (if (empty? restOfList)
      soFar
      (recur
        (cons (first restOfList) soFar)
        (rest restOfList)))))

(defn factorial [n]
  (loop [acc 1
         new-n n]
    (if
      (<= new-n 1)
      acc
      (recur (*' acc new-n) (dec new-n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; destructuring (let, defn, loop)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def s [1 2 3])

;; regular let expression
(let [f (first s)
      r (rest s)]
  (println (str f " and " r)))

;; ... using destructing for vector
(let [[f & r] s]
  (println (str f " and " r)))

;; example of let with destructing on map by key
(def m {:foo 1 :bar 2})
(let [{f :foo} m]
  (println f))

;; combine map and vector destructuring
(def m {:foo [42, 10] :bar 2})
(let [{[a,b] :foo} m]
  (println (+ a b)))

(def sh ["Stephen" "Hawking"])
(let [[first-name last-name :as full-name] sh ]
  {:full full-name :elems {:first first-name :last last-name}})
;; {:full ["Stephen" "Hawking"] :elems {:first "Stephen" :last "Hawking"}}

(let [[first-name last-name :as full-name] sh ]
  {:full full-name :elems {:first first-name :last last-name}})


(let [{:keys [fname sname]} {:fname "John" :sname "McCarthy"}]
  (str sname ", " fname))
;; "McCarthy, John"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(true? (= 4/2 2))
(false? (= 4 5))
(= false (nil? 0))
(not-nil? 42)
(zero? 0)
(empty? nil)
(keyword? :foo)
(symbol? 'foo)
(number? 12)

(= false (= 2.0 2))
(= true (== 2.0 2))

(= true (not (= 1 nil)))

(=
  (or (empty? [1]) (nil? 42) false "foo")
  (and (empty? '()) (nil? nil) true "foo")
  "foo")


(= (list 1 2 3) (vector 1 2 3))

(= :a
   (if true :a
     :b))

(= "foo" (if (> 2 1) "foo"))
(= nil (if false "foo"))

(= :your-road
   (cond
     (= x 1) :road-not-taken
     (= x 2) :another-road-not-taken
     :else :your-road))

(defn describe-exercise [exercise]
  (case exercise
    :bicycling        "pretty fast"
    :jogging          "not super fast"
    :walking          "not fast at all"
    "is that even exercise?"))

(= 'doom (if-not false
           'doom
           'more-doom))

(= nil
   (when false (/ 1 0)))
(= "foo"
   (when true "foo"))

(= nil
   (when-not true "foo"))

(= "foo"
   (when-not false "foo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lazy seq
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= '(1 2) (range 1 3))
(= '(0 1 2) (range 3))
(range 0 6 2) ;; (0 2 4)

(= [0 1 2 3 4 5]
   (take 6 (range 100)))

(= [95 96 97 98 99]
   (drop 95 (range 100)))

(= [:a :a :a :a :a :a :a :a :a :a]
  (repeat 10 :a))
(=
  (repeat 10 :hello)
  (take 10 (iterate identity :hello)))

(= (range 20) (take 20 (iterate inc 0)))

(interpose ", " [1 2 3]) ;; (1 ", " 2 ", " 3)
(apply str (interpose ", " [1 2 3])) ;; "1, 2, 3"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for comprehension
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= '(0 1 2 3)
   (for [x (range 4)] x))

(= '(0 1 4 9)
   (map (fn [x] (* x x)) (range 4))
   (for [x (range 4)] (* x x)))

(= '(1 3 5 7 9)
   (filter odd? (range 10))
   (for [x (range 10) :when (odd? x)]
     x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; partitions, group by
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= '((0 1) (2 3)) (partition 2 (range 4)))
(= '((:a :b :c)) (partition 3 [:a :b :c :d :e]))
(= '((0 1 2) (3 4)) (partition-all 3 (range 5)))
(= '((0 1 2) (5 6 7) (10 11 12)) (partition 3 5 (range 13)))
(= '((0 1 2) (3 4 5) (6 :hello)) (partition 3 3 [:hello] (range 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; namespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; at program start current namespace is user that contain elements from clojure.core

(in-ns 'foo) ;; change currene namespace to foo

;; namespace can have following types of mappings:
;; - symbol to var interned

(def foo 12)

;; - symbol to var referred

(clojure.core/refer 'foo)
(bar) ;; instead of (foo/bar)

;; if there already is foo defined in current namespace we have to rename
;; otherwise ther would be execption
(clojure.core/refer 'foo :rename {stuff foo-stuff magic da-magic-thingi})
(foo-stuff) ;; instead of foo/stuff

;; - symbol to class
;; import macro

(clojure.core/import java.util.Date)

;; - symbol to namespace
(clojure.core/alias 'super 'superlongstuff)
(super/foo) ;; instead of superlongstuff/foo

;; load file myfile.clj from package (relative to current)
(load "mypackage.myfile")

;; load lib from classpath
(require 'clojure.java.io) ;; clojure/java/io.clj
(require '(clojure.java [io :as bar]))  ;; .. and rename

;; load and refer
(use 'clojure.java.io)

;; usually above stuff is used by macro ns
(ns foo.bar
  (:require clojure.contrib.sql)
  (:use clojure.test)
  (:import java.util.Date java.util.Timer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def x (with-meta [1 2] {:foo true}))
(= x [1 2])
(= {:foo true} (meta x))

;; metadata can be added to anything except (nil, numbers, boolean, strings)
;; (def x2 (with-meta 1 {:foo true}))  CompilerException java.lang.ClassCastException: java.lang.Long cannot be cast to clojure.lang.IObj
(def ^{:is-lucky true} x 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; definition
(defmacro my-if-not [condition expr]
  (list 'if condition nil expr))

;; usage
(my-if-not (> 3 4) (println "macro fu is strong in you"))

;; show definition
(macroexpand '(my-if-not (> 3 4) (println 12))) ;; (if (> 3 4) nil (println 12))
(macroexpand '(when 1 2 3 4)) ;; (if 1 (do 2 3 4))

(defmacro my-if-not2
  ([condition expr]
   (list 'if condition nil expr))
  ([condition expr1 expr2]
   (list 'if condition expr2 expr1)))

(my-if-not2 (> 3 4) (println 42) (println "macro is strong in you"))
(my-if-not2 (> 3 4) (println 42))

;; TODO syntax quoting in macros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multimethods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti diet (fn [x] (:eater x)))
(defmethod diet :herbivore [a] (str (:name a) " eats veggies."))
(defmethod diet :carnivore [a] (str (:name a) " eats animals."))
(defmethod diet :default [a] (str "I don't know what " (:name a) " eats."))

(= "Thumper eats veggies."
   (diet {:name "Thumper" :eater :herbivore}))

(= "Simba eats animals."
   (diet {:name "Simba" :eater :carnivore}))

(= "I don't know what John Bravo eats."
   (diet {:name "John Bravo"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def the-world (ref "hello"))
(= "hello"
   (deref the-world)
   @the-world)

;; set value to ref by ref-set
(= "better"
   (do
     (dosync
       (ref-set the-world "better"))
      @the-world))

;; change ref by passing function to alter
(def my-val (ref 42))
(= 44
   (do
     (dosync
       (alter my-val inc)
       (alter my-val inc))
    @my-val))

;; alter can take expression instead of function
(= 20
   (do
     (dosync
       (alter the-world + 20))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def atomic-clock (atom 0))

(= 0 @atomic-clock)
(= 1
   (do
     (swap! atomic-clock inc)
      @atomic-clock))
(= 5
   (do
     (swap! atomic-clock + 4)
     @atomic-clock))

(= 20
   (do
     (compare-and-set! atomic-clock 100 "no way")
     @atomic-clock))

(= 42
   (do
     (compare-and-set! atomic-clock 20 42)
     @atomic-clock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; datatypes: defrecord deftype ;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Nobel [prize])
(= "peace"
   (.prize (Nobel. "peace")))
(= "physics"
   (:prize (Nobel. "physics")))

(deftype Pulitzer [prize])
(= "literature" (.prize (Pulitzer. "literature")))

(= [true false]
   (map map?
        [(Nobel. "chemistry")
         (Pulitzer. "music")]))


(defprotocol Award
  (present [this recipient]))

(defrecord Oscar [category]
  Award
  (present [this recipient]
    (print (str "Congratulations on your "
                (:category this) " Oscar, "
                recipient
                "!"))))

(deftype Razzie [category]
  Award
  (present [this recipient]
    (print
      (str
        "You're really the "
        category
        ", "
        recipient
        "... sorry."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Java interop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(javadoc "warfare")
(= java.lang.String (class "warfare"))
(= "SELECT * FROM" (.toUpperCase "select * from"))
(== 1024 (Math/pow 2 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  clojure.test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clojure.test/is (= 4 (+ 2 2)))
(clojure.test/is (= 4 (+ 2 2 2)) "Addition works")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; threading macros (arrow macros), doto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -> (thread-first macro)
;; pass result as first argument to all next expressions

(= (-> {}
    (assoc :a 1)
    (assoc :b 2))
   {:a 1, :b 2})

(=
  (-> 1
     (+ 1)
     (* 2))
  4)


(first
  (.split
    (.replace
      (.toUpperCase "a b c d")
      "A" "X")
    " "))

(-> "a b c d"
    .toUpperCase
    (.replace "A" "X")
    (.split " ")
    first)

(-> 3 (- 2)) ;; 1

;; ->> (thread-last macro)
;; pass result as first argument to all next expressions

(->> 3 (- 2)) ;; -1

;; doto returns oryginal object and pass the same object to all executions (not result)
(doto 3
  (- 2)
  print) ;; 3 3

(-> 3
  (- 2)
  print) ;; 1 nil

;; http://clojure.org/guides/threading_macros
;; TODO read REPL example: https://clojuredocs.org/clojure.core/-%3E#example-542692ccc026201cdc326c5a

;; some-> not pass expression if it is null

(-> {:a 1} :b inc) ;; NullPointerException
(some-> {:a 1} :b inc) ;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def arr (double-array '(1.0 2.0)))
(aget arr 1) ;; 2.0

(def arr2 (int-array '(1 2)))
(aget arr2 1) ;; 2

;; (aget [1 2 3] 1)
;; IllegalArgumentException No matching method found: aget

;; misc

;; into add all elements to first collection using conj

(into (sorted-map) [[1 2] [3 4]]) ;; {1 2, 3 4}
(into (sorted-map) [{1 2} {3 4}]) ;; {1 2, 3 4}
(into [1 2] '(3 4)) ;; [1 2 3 4]
(into [1 2] [3 4])  ;; [1 2 3 4]
(into '(1 2) [3 4]) ;; (4 3 1 2)

;; conversions & types

(type 3.14)          ;; java.lang.Double
(type (double 3.14)) ;; java.lang.Double
(type (float 3.14))  ;; java.lang.Float

(type 2)             ;; java.lang.Long
(type (long 3.14))   ;; java.lang.Long
(type (int 3.14))    ;; java.lang.Integer
