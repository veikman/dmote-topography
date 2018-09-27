;;; A two-dimensional topography generator for use with OpenSCAD.

(ns dmote-topography.core
  (:require [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.core.matrix :as mtrx]
            [environ.core :refer [env]]
            [unicode-math.core :refer :all])
  (:gen-class :main true))

(defn 𝒩
  "The normal distribution’s probability density function with unicode-math."
  [x ￼￼μ σ]
  (let [v (ⁿ σ 2)]
    (* (/ 1 (√ (* 2 π v)))
       (ⁿ e (- (/ (ⁿ (- x ￼￼μ) 2) (* 2 v)))))))

;; Matrix functions:

(defn- normals
  "Produce a matrix based on a simplified bivariate normal distribution."
  [op cx μx σx cy μy σy {x-extent :x-extent y-extent :y-extent}]
  (mtrx/compute-matrix [x-extent y-extent]
    (fn [y x] (op (* cx (𝒩 x μx σx)) (* cy (𝒩 y μy σy))))))

(defn multiplied-normal-distributions
  [options]
  (let [{cx :x-c μx :x-mu σx :x-sigma
         cy :y-c μy :y-mu σy :y-sigma} options]
    (normals * cx μx σx cy μy σy options)))

(defn additive-normal-distributions
  [options]
  (let [{include :i-include x-extent :x-extent y-extent :y-extent} options
        {cx :i-x-c μx :i-x-mu σx :i-x-sigma} options
        {cy :i-y-c μy :i-y-mu σy :i-y-sigma} options]
    (if include
      (normals + cx μx σx cy μy σy options)
      (mtrx/zero-array [x-extent y-extent]))))

(defn scale-inverted
  [{factor :i-c} matrix]
  (mtrx/mul factor matrix))

(defn pillow
  "A 2D array that rises to a gentle peak. A four-way product of
  logarithms over both axes’ indices. This will intentionally leave
  the extremes of the matrix on all sides at 0."
  [{x-extent :x-extent y-extent :y-extent}]
  (apply mtrx/emul
    (reduce
      (fn [coll f]
        (conj coll
          (mtrx/compute-matrix [x-extent y-extent] #(Math/log (f %1 %2)))))
      []
      [(fn [y x] (+ x 1))
       (fn [y x] (+ y 1))
       (fn [y x] (- x-extent x))
       (fn [y x] (- y-extent y))])))

(defn soften
  "Exponentiate matrix."
  [{n :hardness} matrix]
  (mtrx/pow matrix n))

(defn lift
  "A CLI-controlled addition to all points."
  [{n :lift} matrix]
  (mtrx/emap #(+ % n) matrix))

(defn unit-scale
  "Normalize all elements of a matrix to unit scale, 0-1.
  If no differences exist, return all zeroes."
  [matrix]
  (let [peak (mtrx/emax matrix)
        trough (mtrx/emin matrix)
        Δ (- peak trough)]
    (if (zero? Δ)
      (mtrx/zero-array (mtrx/shape matrix))
      (mtrx/emap (fn [v] (/ (- v trough) Δ)) matrix))))

(defn invert
  "Invert all values of a unit-scaled matrix."
  [matrix]
  (mtrx/emap (fn [v] (- 1 v)) matrix))

(defn normalize
  "Adjust unit-scale values to requested z-axis scale."
  [{peak :z-extent trough :z-min} matrix]
  (let [Δ (- peak trough)]
   (mtrx/emap (fn [v] (+ (* v Δ) trough)) matrix)))

;; Application IO:

(defn print-matrix
  "Print a 2D matrix in the format expected by OpenSCAD’s surface(),
  with information about the source."
  [options matrix]
  (let [{widest :z-extent precision :precision} options
        width (+ (count (str (int widest))) precision 1)
        template (str "%" width "." precision "f")]
    (println (format "# Generated with dmote-topography, version %s. Arguments:"
                     (env :dmote-topography-version)))
    (println "#" (string/join " " *command-line-args*))
    (doseq [line matrix]
      (println (string/join " " (map #(format template (double %)) line))))))

(defn make-matrix [options]
  (->> (multiplied-normal-distributions options)
       (unit-scale)
       (mtrx/add
         (->> (additive-normal-distributions options)
              (invert)
              (unit-scale)
              (scale-inverted options)))
       (unit-scale)
       (lift options)
       (mtrx/emul
         (->> (pillow options)
              (unit-scale)
              (soften options)))
       (unit-scale)
       (normalize options)
       (print-matrix options)))

(def cli-options
  "Define command-line interface."
  [["-x" "--x-extent N" "Nodes on x axis"
    :default 100 :parse-fn #(Integer/parseInt %)]
   ["-y" "--y-extent N" "Nodes on y axis"
    :default 100 :parse-fn #(Integer/parseInt %)]
   ["-z" "--z-extent N" "Peak height"
    :default 100 :parse-fn #(Integer/parseInt %)]
   [nil "--z-min N" "Trough height"
    :default 0.0 :parse-fn #(Float/parseFloat %)]
   [nil "--x-c N" "Coefficient of 𝒩 on x axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--x-mu N" "μ (midpoint) of 𝒩 on x axis"
    :default 0.0 :parse-fn #(Float/parseFloat %)]
   [nil "--x-sigma N" "σ (softness) of 𝒩 on x axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--y-c N" "Coefficient of 𝒩 on y axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--y-mu N" "μ of 𝒩 on y axis"
    :default 0.0 :parse-fn #(Float/parseFloat %)]
   [nil "--y-sigma N" "σ of 𝒩 on y axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--i-include" "Include inverted 𝒩"
    :default false]
   [nil "--i-c N" "Coefficient of inverted 𝒩 versus basic 𝒩"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--i-x-c N" "Coefficient of inverted 𝒩 on x axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--i-x-mu N" "μ (midpoint) of inverted 𝒩 on x axis"
    :default 0.0 :parse-fn #(Float/parseFloat %)]
   [nil "--i-x-sigma N" "σ (softness) of inverted 𝒩 on x axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--i-y-c N" "Coefficient of inverted 𝒩 on y axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--i-y-mu N" "μ of inverted 𝒩 on y axis"
    :default 0.0 :parse-fn #(Float/parseFloat %)]
   [nil "--i-y-sigma N" "σ of inverted 𝒩 on y axis"
    :default 1.0 :parse-fn #(Float/parseFloat %)]
   [nil "--hardness N" "Hardness of edges"
    :default 1 :parse-fn #(Float/parseFloat %)]
   [nil "--lift N" "Extra height added before softening edges"
    :default 0 :parse-fn #(Float/parseFloat %)]
   ["-p" "--precision N" "Printing precision in final result"
    :default 2 :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]])

(defn -main [& raw]
  (let [args (parse-opts raw cli-options)
        options (:options args)]
   (cond
     (some? (:errors args)) (do (println (first (:errors args)))
                                (println (:summary args))
                                (System/exit 1))
     (:help options) (println (:summary args))
     :default (make-matrix options))))
