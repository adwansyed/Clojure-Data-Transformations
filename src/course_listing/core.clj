(ns course-listing.core
  (:gen-class)
  (:require
    [clojure.data.csv :as csv]
	[course-listing.task1]
	[course-listing.task2]
	[course-listing.task3]))

(defn -main [task course-listing-file]
  (case task
	"task1" (course-listing.task1/main course-listing-file)
	"task2"	(course-listing.task2/main course-listing-file)
	"task3"	(course-listing.task3/main course-listing-file)
	"default" (println "usage: [task#] [path-to-file]")))
