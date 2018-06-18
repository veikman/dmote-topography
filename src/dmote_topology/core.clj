;;; A two-dimensional topology generator for use with OpenSCAD.

(ns dmote-topology.core
  (:require [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [environ.core :refer [env]]
            [unicode-math.core :refer :all])
  (:gen-class :main true))

(defn 𝒩
  "The normal distribution’s probability density function with unicode-math."
  [x ￼￼μ σ]
  (let [v (ⁿ σ 2)]
    (* (/ 1 (√ (* 2 π v)))
       (ⁿ e (- (/ (ⁿ (- x ￼￼μ) 2) (* 2 v)))))))

(defn make-matrix
  "Produce a matrix based on a simplified bivariate normal distribution."
  [{x-extent :x-extent y-extent :y-extent
    cx :x-c μx :x-mu σx :x-sigma
    cy :y-c μy :y-mu σy :y-sigma}]
  (into []
    (for [y (range y-extent)]
      (into []
        (for [x (range x-extent)]
          (+ (* cx (𝒩 x μx σx)) (* cy (𝒩 y μy σy))))))))

(defn normalize-matrix
  "Invert and adjust all values to requested z-axis scale."
  [{peak1 :z-extent trough1 :z-min} matrix]
  (let [peak0 (apply max (map (partial apply max) matrix))
        trough0 (apply min (map (partial apply min) matrix))]
    (into []
      (for [y (range (count matrix))]
        (into []
          (for [x (range (count (nth matrix y)))]
            (let [unit-scale (/ (- (get-in matrix [y x]) trough0)
                                (- peak0 trough0))
                  inverted (- 1 unit-scale)]
             (+ (* inverted (- peak1 trough1)) trough1))))))))

(defn print-matrix
  "Print a 2D matrix in the format expected by OpenSCAD’s surface(),
  with information about the source."
  [options matrix]
  (let [{widest :z-extent precision :precision} options
        width (+ (count (str (int widest))) precision 1)
        template (str "%" width "." precision "f")]
    (println (format "# Generated with dmote-topology, version %s, settings %s."
                     (env :dmote-topology-version)
                     (into (sorted-map) options)))
    (doseq [line matrix]
      (println (string/join " " (map #(format template (double %)) line))))))

(def cli-options
  "Define command-line interface."
  [["-x" "--x-extent N" "Nodes on x axis"
    :default 100 :parse-fn #(Integer/parseInt %)]
   ["-y" "--y-extent N" "Nodes on y axis"
    :default 100 :parse-fn #(Integer/parseInt %)]
   ["-z" "--z-extent N" "Peak height"
    :default 100 :parse-fn #(Integer/parseInt %)]
   [nil "--z-min N" "Trough height"
    :default 0 :parse-fn #(Float/parseFloat %)]
   [nil "--x-c N" "Coefficient of 𝒩 on x axis"
    :default 1 :parse-fn #(Float/parseFloat %)]
   [nil "--x-mu N" "μ (midpoint) of 𝒩 on x axis"
    :default 0 :parse-fn #(Float/parseFloat %)]
   [nil "--x-sigma N" "σ (softness) of 𝒩 on x axis"
    :default 1 :parse-fn #(Float/parseFloat %)]
   [nil "--y-c N" "Coefficient of 𝒩 on y axis"
    :default 1 :parse-fn #(Float/parseFloat %)]
   [nil "--y-mu N" "μ of 𝒩 on y axis"
    :default 0 :parse-fn #(Float/parseFloat %)]
   [nil "--y-sigma N" "σ of 𝒩 on y axis"
    :default 1 :parse-fn #(Float/parseFloat %)]
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
     :default (->> (make-matrix options)
                   (normalize-matrix options)
                   (print-matrix options)))))
