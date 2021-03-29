(ns seven-guis.cells
  (:require [reagent.core :as r]
            [reagent.dom :as rd]))

(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def correspondence
  (let [numbers (range 1 27)
        one-side (concat letters numbers)
        other-side (concat numbers letters)]
    (zipmap one-side other-side)))



(defn empty-cell [] (r/atom {:previous-formula ""
                             :formula ""
                             :calculated-value nil
                             :dependencies #{}
                             :dependents #{}
                             :active false
                             :dirty false
                             :error nil}))

(def cells-data
  (vec
   (for [row (range 100)]
     (vec
      (concat '("placeholder")
              (for [col (range 26)]
                (empty-cell)))))))

(defn atom-from-ref [[i j]]
  (get-in cells-data [i j]))

(defn resolve-reference [r]
  (cond (string? r)
        [(js/Number (apply str (rest r))) (correspondence (first r))]

        (vector? r)
        (str (correspondence(second r)) (first r))))


(def digits #{1 2 3 4 5 6 7 8 9 0})
(def ref-re #"[A-z]\d+")
(def num-re #"\d*\+?\d+")


(def operator? #{\+ \- \* \/ \( \)})




(defn tokenize [s] ;should be pre-trimmed
  (->> s
       rest
       (partition-by operator?)
       (map #(apply str %))))

(defn set-up-for-eval [])

(defn typeof-input [s]
  (cond
    (re-matches #"\s*" s)
    :empty
    #_{:empty s}
    
    (clojure.string/starts-with? s "=")
    :formula

    #_{:formula (rest s)}  ;treat as formula 

    (re-matches #"[\d\s]+" s)
    :raw
    #_{:raw (js/Number s)} ; treat as raw numerical value ---change so accepts decimals

    :else
    :label
    #_{:label s})) ; treat as a label/string

(defn replace-references [tokens-list]
  (map (fn [token]
         (if (re-matches ref-re token)
           (-> token
               resolve-reference
               atom-from-ref
               deref
               :calculated-value)
           token))
       tokens-list))

(defn detect-cycle [[i j] references]
  (or (references [i j])
      (if-not (empty? references)
        (detect-cycle [i j] (set (mapcat #(-> % atom-from-ref deref :dependencies) references))))))

(defn extract-references [tokenized]
  (->> tokenized
       (filter #(re-matches #"[A-z]\d+" %))
       (map resolve-reference)
       set))

(defn value-of-input [[i j] input]
  (let [input-type (typeof-input input)
          calculated-value (case input-type
                             :raw (js/Number input)
                             :empty nil
                             :label input
                             :formula (let [tokenized (tokenize input)

                                            _ (if (detect-cycle [i j] (extract-references tokenized))
                                                (throw {:type :circular-reference
                                                        :message (str "Circular reference detected" )}))
                                          
                                            references-replaced (replace-references tokenized)

                                            ready-for-js (apply str references-replaced)
                                            result (js/eval ready-for-js)]
                                        result))]
      calculated-value))




(defn update-value! [[i j]]
(swap! (atom-from-ref [i j])
             (fn [{:keys [formula] :as cell}]
               (assoc cell :calculated-value (value-of-input [i j] formula))))
)



(def dirty-leaves (atom #{}))

(defn propagate-dirt! [[i j]]
  (let [cell-ratom (atom-from-ref [i j])
        {:keys [dependents]} @cell-ratom ]

    (swap! cell-ratom assoc :dirty true)
    (if (seq dependents)
      (doseq [d dependents]
        (propagate-dirt! d))
      (swap! dirty-leaves conj [i j]))))

(defn clean-up! [[i j]]
  (let [cell-ratom (atom-from-ref [i j])
        {:keys [dependencies]} @cell-ratom]
    (doseq [d dependencies]
      (if (:dirty @(atom-from-ref d))
        (clean-up! d)))
    (update-value! [i j])
    (swap! cell-ratom assoc :dirty false)
    (swap! dirty-leaves disj [i j])))


;we already have all the nodes, store what depends on them in them
(def active-cell (r/atom nil))


(defn set-up-connections! [[i j] references]
  (let [cell-ratom (atom-from-ref [i j])]
    (swap! cell-ratom assoc :dependencies references)
    (doseq [r references]
      (swap! (atom-from-ref r) update :dependents conj [i j]))))

(defn remove-dependencies! [[i j] dead-dependencies]
  (swap! (atom-from-ref [i j]) update :dependencies clojure.set/difference dead-dependencies)
  (doseq [d dead-dependencies]
    (swap! (atom-from-ref d) update :dependents disj [i j])))



(defn update-dependencies! [[i j] input-type]
  (let [cell-ratom (atom-from-ref [i j])
        {:keys [formula previous-formula
                dependencies]}  @cell-ratom
        
        references (if (= input-type :formula)
                     (extract-references (tokenize formula))
                     #{})]
    
    (set-up-connections! [i j] references)

    (if (:formula previous-formula)
      (->> references
           (clojure.set/difference dependencies )
           (remove-dependencies! [i j])))))


(defn update-dependents! [dependents]
  (doseq [d dependents]
    (propagate-dirt! d))
  (doseq [[i j] @dirty-leaves]
    (clean-up! [i j])))

(defn clean-input! [cell-ratom]
  (swap! cell-ratom update :formula
         #(-> % (clojure.string/replace #"\s" "" )
              (clojure.string/upper-case))))

(defn on-deselect [[i j]]
  (let [cell-ratom (atom-from-ref [i j])
        _ (swap! cell-ratom assoc :error nil)
        _  (swap! cell-ratom update :formula clojure.string/trim)
        input-type (typeof-input (:formula @cell-ratom))
        _ (if-not (= input-type :label) (clean-input! cell-ratom))
        {:keys [formula dependents]} @cell-ratom]

    (try
      (update-value! [i j])

      (update-dependencies! [i j] input-type)
      
      (update-dependents! dependents)
      (swap! cell-ratom assoc :previous-formula {input-type formula})
      
      (catch :default e
        (swap! (atom-from-ref [i j]) assoc :error (or (:message e)
                                                      "Something wrong with the formula"))))))

(defn deselect! [[i j]]
  (reset! active-cell nil)
  (swap! (atom-from-ref [i j])
          assoc :active false)
    (on-deselect [i j]))

(defn set-active! [[i j]]
  (if-let [former-active @active-cell]
    (deselect! former-active))
  (swap! (atom-from-ref [i j]) assoc :active true )
  (reset! active-cell [i j]))

(defn cell-input [formula [i j]]
  (r/create-class
   {:component-did-mount (fn [this]
                           (.focus
                            (rd/dom-node this)))
    :reagent-render (fn [formula]
                      [:input {:value formula
                               :on-change #(swap! (atom-from-ref [i j])
                                                  assoc :formula
                                                  (.. % -target -value))}])}))

(defn cell [_ i j]
  (let [cell-ratom (get-in cells-data [i j])
        {:keys [formula
                previous-formula
                calculated-value
                dependencies
                dependents
                active
                dirty
                error]}
        @cell-ratom]
    [:td {#_#_:key (:key _)
            :on-click #(if-not active
                         (do
                           (set-active! [i j])))}
     (if active
       [cell-input formula [i j]]
       (str #_#_#_(correspondence j)  i ":"
            #_#_formula ":"
            #_#_active ":"
            #_(str dependencies)
            (if error
              error
              calculated-value)))]))

(defn cells []
  [:div

   (if @active-cell
     [:p (str @(atom-from-ref @active-cell))])
   [:style "td, th{
border: black solid 1px;
width: 10em;
}"]
   [:table
    [:thead
     [:tr [:th ""]
      (for [l letters]
        [:th {:key l}
         l])]]
    [:tbody
     (for [i (range 100)]
       [:tr {:key i}
        [:td i]
        (for [j (range 1 27)]
          [cell {:key (str i " " j)} i j])])]]])

(defn between? [n [low high]];all-exclusive
  (and (> high n)
       (< low n)))

(defn move-relative [move]
  (let [current @active-cell
        prospective (map + move current)]
    (if (every? true? (map between?
                           prospective
                           [[-1 100]
                            [0 27]]))
    (set-active! prospective))))


(defn key-react [e]
  (if @active-cell
    (let [k (.-key e)]
      (case k "ArrowUp" (move-relative [-1 0])
            "ArrowRight" (move-relative [0 1])
            "ArrowDown" (move-relative [1 0])
            "ArrowLeft" (move-relative [0 -1])
            "Enter" (move-relative [1 0])
            :default ))))



(defonce kbs (atom nil))

(if-not @kbs
  (do (js/document.addEventListener "keydown" key-react)
      (reset! kbs true)))






