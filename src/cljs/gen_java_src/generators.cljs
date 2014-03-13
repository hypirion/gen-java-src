(ns gen-java-src.generators
  (:require [simple-check.core :as sc]
            [simple-check.generators :as gen]
            [clojure.string :as s]
            [clojure.set :as cset]))

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
  (gen/elements [:= :<= :>= :< :> :!=]))

(defn statement
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
  ;; TODO
  (gen/return "TODO: invoke-gen"))

(defn int-expr*
  [vars avail-met]
  (fn [size]
    (if (zero? size)
      (gen/frequency [[3 small-int]
                      [(zeroed vars 5) (if (seq vars) (gen/elements vars))]])
      (let [new-size (quot size 2)
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

(defn- gen-method-body
  [mgen]
  (->>
   (fn [{:keys [name input locals] :as m}]
     (let [vars (distinct (concat input locals))]
       (->> (gen/tuple (gen/return m) (int-operation vars []))
            (gen/fmap
             (fn [k]
               (let [[m i-op] k]
                 [:method name input [[:declare locals]
                                      [:return i-op]]]))))))
   (gen/bind mgen)))

(defn method
  [svars]
  (->>
   (gen/tuple method-name
              (const-size 4 (private-vars svars))
              (private-vars svars))
   (gen/fmap (fn [[m input local]]
               {:name m, :input (vec input),
                :locals local}))
   gen-method-body))

(def statics-and-methods
  (gen/bind static-vars
            (fn [svars]
              (->>
               (method svars)
               (gen/vector)
               (gen/fmap
                ;; remove methods with same name and same arity
                #(distinct-by (juxt :name (comp count :input)) %))
               (gen/tuple (gen/return svars))))))

(def class-gen
  (gen/fmap (fn [[cname [statics methods]]]
              [:class cname
               (concat [(into [:statics] statics)]
                       methods)])
            (gen/tuple class-name
                       statics-and-methods)))
