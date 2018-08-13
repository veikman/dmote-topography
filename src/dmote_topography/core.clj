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

(defn normal-distributions
  "Produce a matrix based on a simplified bivariate normal distribution."
  [{x-extent :x-extent y-extent :y-extent
    cx :x-c μx :x-mu σx :x-sigma
    cy :y-c μy :y-mu σy :y-sigma}]
  (mtrx/compute-matrix [x-extent y-extent]
    (fn [y x] (+ (* cx (𝒩 x μx σx)) (* cy (𝒩 y μy σy))))))

(defn pillow
  "A 2D array that rises to a gentle peak. A four-way product of
  logarithms over both axes’ indices. This will intentionally leave
  the extremes of the matrix on all sides at 0."
  [{x-extent :x-extent y-extent :y-extent}]
  (apply mtrx/emul
    (reduce
      (fn [coll f] (conj coll (mtrx/compute-matrix
                                [x-extent y-extent] #(Math/log (f %1 %2)))))
      []
      [(fn [y x] (+ x 1))
       (fn [y x] (+ y 1))
       (fn [y x] (- y-extent x))
       (fn [y x] (- y-extent y))])))

(defn unit-scale
  "Normalize all elements of a matrix to unit scale, 0-1."
  [matrix]
  (let [peak (mtrx/emax matrix)
        trough (mtrx/emin matrix)
        Δ (- peak trough)]
   (mtrx/emap (fn [v] (/ (- v trough) Δ)) matrix)))

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
    (println (format "# Generated with dmote-topography, version %s, settings %s."
                     (env :dmote-topography-version)
                     (into (sorted-map) options)))
    (doseq [line matrix]
      (println (string/join " " (map #(format template (double %)) line))))))

(defn make-matrix [options]
  (->> (normal-distributions options)
       (unit-scale)
       (invert)
       (mtrx/emul (unit-scale (pillow options)))
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
