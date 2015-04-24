(ns bot.core
  (:gen-class)
  (:require [clojure.string :as str]))

(def all-messages (atom false))
(def message (atom nil))
(def planets (atom []))
(def friends (atom []))
(def id (atom 0))

(defn add-planet [pid x y incr id fleet]
  (swap! planets conj {:pid pid :x x :y y :incr incr :id id :fleet fleet}))

(defn clear-planets []
  (reset! planets []))

(defn clear-friends []
  (reset! friends []))

(defn recv-message [msg]
  (if (= msg @id)
    (reset! all-messages true)
    (do
      (swap! friends conj msg)
      (reset! message msg))))

(defn set-id [player-id]
  (reset! id player-id))

(defn divide-pasific [planet]
  (cond (= (:id planet) @id) :my
        (= (:id planet) 0 ) :target))

(defn divide-aggressive [planet]
  (cond (= (:id planet) @id) :my
        (not-any? #(= (:id planet) %) @friends) :target))

(defn distance [planet1 planet2]
  (+ ( #(* % %) (- (:x planet1) (:x planet2) ) )
     ( #(* % %) (- (:y planet1) (:y planet2) ) )))

(defn cost-distance [planet1 planet2]
  (let [square #(* % %)]
    (/ (+ (square (- (:x planet1) (:x planet2) ) )
          (square (- (:y planet1) (:y planet2) ) ))
       (square (:incr planet2)))))

(defn compare-distanse [planet planet1 planet2]
  (< (cost-distance planet planet1)
     (cost-distance planet planet2)))

(defn find-camp-planet [from to count camp-planets]
  (let [min-x (min (:x from) (:x to))
        max-x (max (:x from) (:x to))
        min-y (min (:y from) (:y to))
        max-y (max (:y from) (:y to))
        w-size (/ (- max-x min-x) 4)
        h-size (/ (- max-y min-y) 4)
        camp-min-x (+ min-x w-size)
        camp-max-x (- max-x w-size)
        camp-min-y (+ min-y h-size)
        camp-max-y (- max-y h-size)
        camp-planet (some #(when (and (>= (:x %) camp-min-x )
                                      (<= (:x %) camp-max-x )
                                      (>= (:y %) camp-min-y)
                                      (<= (:y %) camp-max-y)) %) camp-planets)]
    (if (nil? camp-planet)
      (println "F" (:pid from) (:pid to) count)
      (println "F" (:pid from) (:pid camp-planet) count))))

(defn send-units [from planets camp-planets]
  (when (not-empty planets)
    (let [const (int (/ (:fleet from) (count planets) ))
          addon (rem (:fleet from) (count planets) )
          planets (sort (partial compare-distanse from) planets)]
      (loop [const const addon addon planets planets]
        (when (and (or (> const 0)  (> addon 0) )
                   (not= planets [] ))
          (find-camp-planet from
                            (first planets)
                            (+ const (if (> addon 0) 1 0 ))
                            camp-planets)
          (recur const (dec addon) (rest planets)))))))

(defn process-planets [my-planets target-planets camp-planets]
  (doseq [from my-planets]
    (send-units from target-planets camp-planets)))

(defn process-step []
  (let [division (group-by (if @all-messages
                           divide-aggressive
                           divide-pasific) @planets)
        my-planets (:my division)
        target-planets (:target division)
        camp-planets (concat my-planets target-planets)]
    (process-planets my-planets target-planets camp-planets))
  (when-not @all-messages
    (println "M" (if @message @message @id)))
  (println ".")
  (clear-planets))

(defn parse [line]
  (if (= line ".")
    (process-step)
    (let [[command & str-args] (str/split line #" ")
          args (map #(Integer. %) str-args)]
      (case command
        "P" (apply add-planet args)
        "M" (apply recv-message args)
        "Y" (apply set-id args)
        (println "Bad command")))))

(defn -main [& args]
  (loop [line (read-line)]
    (when-not (some #(= % line) ["exit" "quit" "done" "end" "q"])
      (parse line)
        (recur (read-line)))))

