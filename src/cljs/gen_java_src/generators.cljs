(ns gen-java-src.generators
  (:require [simple-check.core :as sc]
            [simple-check.generators :as gen]
            [clojure.string :as s]
            [clojure.set :as cset]))

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
  (const-size 4 base-name))

(def static-vars
  "Generates a set of static variables."
  (gen/fmap set (gen/vector var-name)))

(defn private-var
  "Generates a set of local variables (for methods only) which doesn't shadow
  the static ones."
  [svars]
  (gen/such-that #(not (contains? svars %)) var-name))

(defn private-vars
  [svars]
  (gen/fmap set (gen/vector (private-var svars))))

(def small-non-neg-int
  (gen/choose 0 100))

(def small-int
  (gen/choose -100 100))

(def class-gen
  (gen/fmap (fn [cname] {:name cname
                        :methods #{}
                        :static-vars #{}})
            class-name))
