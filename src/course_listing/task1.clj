(ns course-listing.task1
  (:gen-class)
  (:require [course-listing.csv-helper :refer :all]))

(defn print-result [result]
  (mapv (fn [{:keys [code title]}] (println code title)) result))

(defn filter-courses [pred-fn courses]
  (filter
	(fn [courses] (let [semester (:semester courses)
						matched-semester (Integer/parseInt (re-find #"\d\d\d\d\d\d" semester))]
					(pred-fn matched-semester)))
	courses))

(defn sort-courses-by [course-key courses]
  (sort-by course-key courses))

(defn main [course-listing-file]
  (let [file-contents (slurp course-listing-file)]
	(->> file-contents
	  (create-map-from-file)
	  (filter-courses #(= % 201709))
	  (sort-courses-by :code)
	  (print-result))))