(ns course-listing.task3
  (:gen-class)
  (:require [course-listing.csv-helper :refer :all]))

(defn print-result [result]
  (mapv (fn [[[first-instructor second-instructor] meeting-count]]
		 (printf "%-30s %-30s %-30d\r\n" first-instructor second-instructor meeting-count)
		 (flush))
	result))

(defn filter-invalid-instructors [courses]
  (filter
	(fn [{:keys [instructor room]}]
	  (let [instructor-trent-university	(re-find #"Trent University" instructor)
			instructor-tba	(re-find #"TBA" instructor)
			invalid-room	(re-find #"N/A" room)]
		(and (nil? instructor-trent-university) (nil? instructor-tba))))
	courses))

(defn get-top-instructors [instructors]
  (take 10 (reverse (sort-by second (frequencies instructors)))))

(defn same-weekday [first-instructor second-instructor]
  (let [first-weekday  (:weekday first-instructor)
		second-weekday (:weekday second-instructor)]
	(= first-weekday second-weekday)))

(defn start-end-hour-relationship [first-instructor second-instructor]
  (let [first-starthour  (:starthour first-instructor)
		first-endhour    (:endhour first-instructor)
		second-starthour (:starthour second-instructor)
		second-endhour   (:endhour second-instructor)]
	(and
	  (or (= first-starthour second-endhour) (= first-endhour second-starthour))
	  (not (= first-starthour second-starthour)))))

(defn same-semester [first-instructor second-instructor]
  (let [first-semester  (:semester first-instructor)
		second-semester (:semester second-instructor)]
	(= first-semester second-semester)))

(defn same-room [first-instructor second-instructor]
  (let [first-room  (:room first-instructor)
		second-room (:room second-instructor)]
	(= first-room second-room)))

(defn not-same-instructor [first-instructor second-instructor]
  (let [first-instructor  (:instructor first-instructor)
		second-instructor (:instructor second-instructor)]
	(not (= first-instructor second-instructor))))

(defn compare-instructors [first-instructor second-instructor]
  (and
	(same-weekday first-instructor second-instructor)
	(start-end-hour-relationship first-instructor second-instructor)
	(same-semester first-instructor second-instructor)
	(same-room first-instructor second-instructor)
	(not-same-instructor first-instructor second-instructor)))

(defn check-for-matches [matched-instructors current-course course-to-match]
  (let [match? (compare-instructors course-to-match current-course)]
	(if match?
	  (conj matched-instructors [(:instructor course-to-match) (:instructor current-course)])
	  matched-instructors)))

(defn find-instructor-meetings [courses]
  (loop [course    (first courses)
		 remaining (rest courses)
		 meetings  []]
	(if-not (empty? remaining)
	  (let [matched (reduce #(check-for-matches %1 %2 course) [] remaining)]
		(recur
		  (first remaining)
		  (rest remaining)
		  (if (empty? matched)
			meetings
			(into meetings matched))))
	  meetings)))

(defn main [course-listing-file]
  (let [file-contents (slurp course-listing-file)]
	(println "Finding pairs of instructors...")
	(->> file-contents
	  (create-map-from-file)
	  (filter-invalid-instructors)
	  (find-instructor-meetings)
	  (get-top-instructors)
	  (print-result))))