(ns flappy-bird-demo.debug
  (:require
   [cljs.reader :as reader]))



(defonce past-states (atom '()))

(defn remember-past-states [_ st old new]
  (swap! past-states #(-> % (conj new) distinct)) )

(defn debugging-pane [state-atom time-loop]

  [:div.debugging-pane

   [:button  {:href "#" :onClick (fn [e]
                                   (reset! state-atom (nth @past-states 20))
                                   (time-loop)) }  "Go Back"]
   ;; [:button {:href "#" :onClick #(reset! state-atom (nth @past-states 30)) } "<<")]
   ;; [:button {:href "#" :onClick #(reset! state-atom (nth @past-states 30)) } ">>")]

   ;; (map-indexed (fn [i s] [:button  {:href "#" :onClick #(reset! state-atom s) }
   ;;                         (str (* 5 (inc i)))]  )
   ;;              (reverse (take 10 (take-nth 5 @past-states))) )

   [:div {:style {:color "white" :width "500px"} }
    [:pre {:id "curstate" }] (prn-str @state-atom)]
   [:textarea {:id "newstate" :style {:height "100px" :width "500px"}}]
   [:button  {:href "#"
         :onClick
         (fn [e]
           (.log js/console (.-value (.getElementById js/document "newstate")))
           (reset! state-atom (merge @state-atom (reader/read-string
                                                  (.-value (.getElementById js/document "newstate")))))

           (time-loop)) }
    "Re-set State"] ])
