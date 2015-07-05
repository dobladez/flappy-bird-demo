(ns flappy-bird-demo.core
  (:require
   [sablono.core :as sab :include-macros true]
   [figwheel.client :as fw]
   [cljs.reader :as reader]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]]
   [flappy-bird-demo.debug :as d])
  (:require-macros [cljs.core.async.macros :refer [go-loop go]]
                   [flappy-bird-demo.macros :refer [spy]]))

(enable-console-print!)

(def horiz-vel -0.15)
(def gravity 0.05)
(def jump-vel 21)
(def start-y 312)
(def start-x 212)
(def bottom-y 561)
(def flappy-width 57)
(def flappy-height 41)
(def pillar-spacing 324 #_324)
(def pillar-gap 160)
(def pillar-width 86)

(def debugging? true) ;;;

(def starting-state { :timer-running false
                      :jump-count 0
                      :initial-vel 0
                      :start-time 0
                      :flappy-start-time 0
                      :flappy-y start-y
                      :flappy-x start-x
                      :pillar-list
                      [{ :start-time 0
                         :pos-x 900
                         :cur-x 900
                         :gap-top 200 }]})

(defn reset-state [_ cur-time]
  (-> starting-state
      (update-in [:pillar-list] (fn [pls] (map #(assoc % :start-time cur-time) pls)))
      (assoc
          :start-time cur-time
          :flappy-start-time cur-time
          :timer-running true)))

(defonce flap-state (atom starting-state))

(defn floor [x] (.floor js/Math x))

(defn translate [start-pos vel time]
  (floor (+ start-pos (* time vel))))

(defn curr-pillar-pos [cur-time {:keys [pos-x start-time] }]
  (translate pos-x horiz-vel (- cur-time start-time)))

(defn in-pillar? [st {:keys [cur-x]}]
  (and (>= (+ (:flappy-x st) flappy-width)
           cur-x)
       (< (:flappy-x st) (+ cur-x pillar-width))))

(defn in-pillar-gap? [{:keys [flappy-y]} {:keys [gap-top]}]
  (and (< gap-top flappy-y)
       (> (+ gap-top pillar-gap)
          (+ flappy-y flappy-height))))

(defn bottom-collision? [{:keys [flappy-y]}]
  (>= flappy-y (- bottom-y flappy-height)))

(defn collision? [{:keys [pillar-list] :as st}]
  (if (some #(or (and (in-pillar? st %)
                      (not (in-pillar-gap? st %)))
                 (bottom-collision? st)) pillar-list)
    (assoc st :timer-running false)
    st))

(defn new-pillar [cur-time pos-x]
  {:start-time cur-time
   :pos-x      pos-x
   :cur-x      pos-x
   :gap-top    (+ 60 (rand-int (- bottom-y 120 pillar-gap)))})

(defn update-pillars [{:keys [pillar-list cur-time] :as st}]
  (let [pillars-with-pos (map #(assoc % :cur-x (curr-pillar-pos cur-time %)) pillar-list)
        pillars-in-world (sort-by
                          :cur-x 
                          (filter #(> (:cur-x %) (- pillar-width)) pillars-with-pos))]
    (assoc st
      :pillar-list
      (if (< (count pillars-in-world) 3)
        (conj pillars-in-world
              (new-pillar
               cur-time
               (+ pillar-spacing
                  (:cur-x (last pillars-in-world)))))
        pillars-in-world))))

;;;
(defn sine-wave [st]
  (-> st
      (update-in [:flappy-y] + (* 6 (.cos js/Math
                                          (/ (:time-delta st)
                                             300)))) ) )

(defn update-flappy [{:keys [time-delta initial-vel flappy-y jump-count] :as st}]
  (if (pos? jump-count)
    (let [cur-vel (- initial-vel (* time-delta gravity))
          new-y   (- flappy-y cur-vel)
          new-y   (if (> new-y (- bottom-y flappy-height))
                    (- bottom-y flappy-height)
                    new-y)]
      (assoc st :flappy-y new-y))

    (sine-wave st)))

(defn score [{:keys [cur-time start-time] :as st}]
  (let [score (- (.abs js/Math (floor (/ (- (* (- cur-time start-time) horiz-vel) 544)
                               pillar-spacing)))
                 4)]
  (assoc st :score (if (neg? score) 0 score))))

(defn time-update [timestamp-inc state]
  (-> state
      (assoc
          :cur-time (+ timestamp-inc (:cur-time state))
          :time-delta (- (+ timestamp-inc (:cur-time state)) (:flappy-start-time state)))
      update-flappy
      update-pillars
      collision? ;;;
      score))

(defn jump [{:keys [cur-time jump-count] :as state}]
  (-> state
      (assoc
          :jump-count (inc jump-count)
          :flappy-start-time cur-time
          :initial-vel jump-vel)))

;; derivatives

(defn border [{:keys [cur-time] :as state}]
  (-> state
      (assoc :border-pos (mod (translate 0 horiz-vel cur-time) 23))))

(defn pillar-offset [{:keys [cur-time]} {:keys [gap-top] :as p}]
  (assoc p
    :upper-height gap-top
    :lower-height (- bottom-y gap-top pillar-gap)))

(defn pillar-offsets [state]
  (update-in state [:pillar-list]
             (fn [pillar-list]
               (map (partial pillar-offset state)
                    pillar-list))))

(defn world [state]
  (-> state
      border
      pillar-offsets))

(defn px [n] (str n "px"))

(defn pillar [{:keys [cur-x pos-x upper-height lower-height]}]
  [:div.pillars
   [:div.pillar.pillar-upper {:style {:left (px cur-x)
                                       :height upper-height}}]
   [:div.pillar.pillar-lower {:style {:left (px cur-x)
                                       :height lower-height}}]])


(defn time-loop [;time
                 ]
  (let [new-state (swap! flap-state (partial time-update 40))]
    (when (:timer-running new-state)
      (go
       (<! (timeout 30))
       (.requestAnimationFrame js/window #(time-loop (+ 30 )))))))

(defn start-game []
  (.requestAnimationFrame
   js/window
   (fn [time]
     (reset! flap-state (reset-state @flap-state 0;time
                                     ))
     (time-loop #_ time))))

(defn main-template [{:keys [score cur-time jump-count
                             timer-running border-pos
                             flappy-y flappy-x  pillar-list] :as st}]
  (sab/html [:div [:div.board { :onMouseDown (fn [e]
                                               (swap! flap-state jump)
                                               (.preventDefault e))}
                   [:h1.score score]
                   (if-not timer-running
                     [:a.start-button {:onClick #(start-game)}
                      (if (< 1 jump-count) "Restart" "Start")]
                     [:span])
                   [:div (map pillar pillar-list)
                    ]
                   [:div.flappy {:style {:top (px flappy-y) :left (px flappy-x)}}]
                   [:div.scrolling-border {:style { :background-position-x (px border-pos)}}] ]

             [:div (when debugging? (d/debugging-pane flap-state time-loop))] ] ))


(let [node (.getElementById js/document "board-area")]
  (defn renderer [full-state]
    (.renderComponent js/React (main-template full-state) node)))

(add-watch flap-state :renderer (fn [_ _ _ n]
                                  (renderer (world n))))

(add-watch flap-state :debug-pane d/remember-past-states)

(reset! flap-state @flap-state)

(fw/start { :on-jsload (fn []
                         ;; you would add this if you
                         ;; have more than one file
                         #_(reset! flap-state @flap-state)
                         )})

