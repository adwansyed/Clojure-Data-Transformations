(ns course-listing.task2
  (:gen-class)
  (:require [course-listing.csv-helper :refer :all]))

(defn print-top-instructors [result]
  (mapv (fn [[instructor lecture-count]]
		  (printf "%-30s %-30d\r\n" instructor lecture-count)) result))

(defn print-courses [result]
  (mapv (fn [{:keys [code title]}] (println code title)) result))

(defn filter-courses [semester-pred courses]
  (filter
	(fn [{:keys [semester schtype instructor]}]
	  	(let [matched-semester	(Integer/parseInt (re-find #"\d\d\d\d" semester))
			  matched-schtype	(re-find #"(?i)Lecture" schtype)
			  instructor-tba	(re-find #"TBA" instructor)]
		  (and (semester-pred matched-semester)
				matched-schtype
				(nil? instructor-tba))))
	courses))

(defn aggregate-top-instructors [courses]
  (->> courses
	(reduce #(update % (%2 :instructor) (fnil conj #{}) (%2 :schtype)) {})
	(reduce-kv #(assoc %1 %2 (count %3)) {})))

(defn get-top-instructors [instructors]
  (take 10 (reverse (sort-by last instructors))))

(defn main [course-listing-file]
  (let [file-contents (slurp course-listing-file)]
	(->> file-contents
	  (create-map-from-file)
	  (filter-courses #(>= % 2015))
	  (print-courses))
	(->> file-contents
	  (create-map-from-file)
	  (filter-courses #(= % 2016))
	  (aggregate-top-instructors)
	  (get-top-instructors)
	  (print-top-instructors))))