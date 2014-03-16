(ns gen-java-src.generators
  (:require [simple-check.core :as sc]
            [simple-check.generators :as gen]
            [clojure.string :as s]
            [clojure.set :as cset]))

(defn maplist
  [f s]
  (if-let [s (seq s)]
    (lazy-seq (cons (f s)
                    (maplist f (next s))))))

(defn- vec-contains?
  [v s]
  (some #(= s %) v))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll, where elt is dropped if
  (f elt) has already been returned by some other element."
  [f coll]
    (let [step (fn step [xs seen]
                   (lazy-seq
                    ((fn [[x :as xs] seen]
                       (when-let [s (seq xs)]
                         (let [f-res (f x)]
                           (if (contains? seen f-res)
                             (recur (rest s) seen)
                             (cons x (step (rest s) (conj seen f-res)))))))
                     xs seen)))]
      (step coll #{})))

(defn- zeroed
  "If (seq m) is nil, return 0, else n."
  [m n]
  (if (seq m) n 0))

(def char-alpha
  "Generate alphabetic characters."
  (gen/fmap cljs.core/char
        (gen/one-of [(gen/choose 65 90)
                     (gen/choose 97 122)])))

(def string-alpha
  "Generate alphabetic strings."
  (gen/fmap s/join (gen/vector char-alpha)))

(defn const-size
  "Generates a generator with constant size."
  [n g]
  (gen/sized
   (fn [size]
     (gen/resize n g))))

(def ^:private base-name
  (->> string-alpha
       gen/not-empty
       ;; I don't know JavaScript, so this is probably not idiomatic
       (gen/such-that #(not (contains? (set "1234567890") (first %))))))

(def method-name
  (->> (gen/fmap s/lower-case base-name)
       (const-size 10)))

(def class-name
  (->> (gen/fmap s/capitalize base-name)
       (const-size 10)))

(def var-name
  (const-size 4 (gen/fmap s/lower-case base-name)))

(def static-vars
  "Generates a set of static variables."
  (gen/fmap distinct (gen/vector var-name)))

(defn private-var
  "Generates a set of local variables (for methods only) which doesn't shadow
  the static ones."
  [svars]
  (gen/such-that #(not (vec-contains? svars %)) var-name))

(defn private-vars
  [svars]
  (gen/fmap distinct (gen/vector (private-var svars))))

(def small-pos-int
  (gen/choose 1 100))

(def small-int
  (gen/choose -100 100))

(declare int-expr)

(def comparison-op
  (gen/elements [:== :<= :>= :< :> :!=]))

(defn gen-comparison
  [vars avail-met]
  (let [iexpr (int-expr vars avail-met)]
    (gen/tuple (gen/return :compare) comparison-op iexpr iexpr)))

(def operation-op
  (gen/frequency [[10 (gen/return :+)]
                  [10 (gen/return :-)]
                  [10 (gen/return :*)]
                  [10 (gen/return :div)]
                  [3 (gen/return :%)]
                  [1 (gen/return :xor)]
                  [1 (gen/return :and)]
                  [1 (gen/return :ior)]]))

(defn int-operation
  [vars avail-met]
  (let [iexpr (int-expr vars avail-met)]
    (gen/tuple (gen/return :int-op) operation-op iexpr iexpr)))

(defn invoke-gen [vars avail-met]
  (if (seq avail-met)
    (gen/bind
     (gen/elements avail-met)
     (fn [m]
       (let [i-ex (int-expr vars avail-met)]
         (->>
          (apply gen/tuple (gen/return (:name m))
                 (repeat (count (:input m)) i-ex))
          (gen/fmap #(into [:invoke] %))))))
    (gen/return "Called invoke-gen even though that should be impossible.")))

(defn int-expr*
  [vars avail-met]
  (fn [size]
    (if (zero? size)
      (gen/frequency [[3 small-int]
                      [(zeroed vars 5) (if (seq vars) (gen/elements vars))]])
      (let [new-size (int (quot size 1.5))
            resize (fn [gen] (gen/resize new-size gen))]
        (gen/frequency [[(zeroed avail-met 1) (resize (invoke-gen vars avail-met))]
                        [2 (resize (int-operation vars avail-met))]
                        [3 small-int]
                        [(zeroed vars 5) (if (seq vars)
                                           (resize (gen/elements vars)))]])))))

(defn int-expr
  [vars avail-met]
  (gen/sized (int-expr* vars avail-met)))

(defn ret-expression
  [vars avail-met]
  (gen/fmap (fn [expr] [:return expr])
            (int-expr vars avail-met)))

(defn gen-assignment
  [vars rmethods]
  (->>
   (gen/tuple (gen/elements vars)
              (gen/frequency [[5 (gen/return :=)]
                              [1 operation-op]])
              (int-expr vars rmethods))
   (gen/fmap
     (fn [vals]
       (into [:assignment] vals)))))

(declare gen-statements)

(defn gen-if
  [vars rmethods]
  (fn [size]
    (let [new-size (int (quot size 1.5))
          gen-stat (gen/resize new-size (gen-statements vars rmethods))]
      (->>
       (gen/tuple (gen-comparison vars rmethods) gen-stat gen-stat)
       (gen/fmap (fn [vals] (into [:if] vals)))))))

(defn gen-statement
  [vars rmethods]
  (gen/frequency [[5 (gen-assignment vars rmethods)]
                  [1 (gen/sized (gen-if vars rmethods))]]))

(defn gen-statements
  [vars rmethods]
  (if (seq vars)
    (gen/vector (gen-statement vars rmethods))
    (gen/return [])))

(defn- gen-method-bodies
  [method-vec]
  (->>
   (fn [method-vec]
     (->> method-vec
          (maplist
           (fn [[{:keys [name input locals statics] :as m} & rmethods]]
             (let [vars (distinct (concat input locals statics))]
               (->> (gen/tuple (gen/return m)
                               (gen-statements vars rmethods)
                               (ret-expression vars rmethods))
                    (gen/fmap
                     (fn [[m statements ret]]
                       [:method name input
                        (-> [[:declare locals]]
                            (into statements)
                            (conj ret))]))))))
          (apply gen/tuple)))
   (gen/bind method-vec)))

(defn method
  [svars]
  (->>
   (gen/tuple method-name
              (const-size 4 (private-vars svars))
              (private-vars svars))
   (gen/fmap (fn [[m input local]]
               {:name m, :input (vec input),
                :locals local, :statics svars}))))

(def statics-and-methods
  (gen/bind static-vars
            (fn [svars]
              (->>
               (method svars)
               (gen/vector)
               (gen/fmap
                ;; remove methods with same name and same arity
                #(distinct-by (juxt first (comp count second)) %))
               gen-method-bodies
               (gen/tuple (gen/return svars))))))

(def class-gen
  (gen/fmap (fn [[cname [statics methods]]]
              [:class cname
               (concat [(into [:statics] statics)]
                       methods)])
            (gen/tuple class-name
                       statics-and-methods)))
