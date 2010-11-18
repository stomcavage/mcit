;     Program: shark.clj
;      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
; Description: Using clojure agents to simulate a 1D ocean of sharks.
;              Start the simulation running by calling (start [num-sharks]).
;        Date: October, 2010

;-----------------------------------------------------------------------------
; Define sharks (as structs, not refs) and their behavior
;-----------------------------------------------------------------------------

(defstruct shark :id :direction :weight :hunger :status)

(defn make-shark
  "Defines a shark with :id = n and various attributes."
  [n]
  (struct-map shark
    :id n
    :direction (if (< (rand) 0.5) :left :right )
    :weight  (+ 90 (* 20 (rand)))
    :hunger 0
    :status :alive))

(defn get-hunger
  "Returns a shark's current hunger."
  [shark]
  (shark :hunger))

(defn get-id
  "Returns a shark's id."
  [shark]
  (shark :id))

(defn get-weight
  "Returns a shark's current weight."
  [shark]
  (shark :weight))

(defn swimming-left?
  "Returns true if the shark's swimming direction is to the left"
  [shark]
  (= (shark :direction) :left))

(defn swimming-right?
  "Returns true if the shark's swimming direction is to the right"
  [shark]
  (= (shark :direction) :right))

(defn starved?
  "Returns true if the shark hasn't eaten in 10 or more moves"
  [shark]
  (>= (shark :hunger) 10))

(defn dead?
  "Returns true if the shark is dead"
  [shark]
  (= (shark :status) :dead))

(defn can-eat?
  "Returns true if the shark can eat this food."
  [shark food]
  (cond
    (or (empty? food) (empty? shark)) false
    :else (or (dead? food) (> (get-weight shark) (get-weight food)))))

(defn eat
  "Returns a heavier copy of the shark."
  [shark food]
  (assoc shark 
    :weight (+ (shark :weight) (* 0.25 (food :weight)))
    :hunger 0))

(defn make-hungrier
  "Returns a hungrier copy of the shark."
  [shark]
  (assoc shark :hunger (inc (shark :hunger))))

(defn die
  "Returns a dead copy of the shark."
  [shark]
  (assoc shark :status :dead :hunger 0))

(defn str-shark
  "Returns a string representation of a shark (or () for an empty list)."
  [shark]
  (cond
    (= shark ()) "()"
    (swimming-left? shark) (concat "<" (str (shark :id)))
    :else  (concat (str (shark :id)) ">")))

;-----------------------------------------------------------------------------
; Define the ocean and its behavior
;-----------------------------------------------------------------------------

(defn make-ocean 
  "Makes a list of a mix of num-sharks actual sharks and some empty locations. 
   All locations in the ocean hold references."
  [num-sharks] 
    (letfn [(add-shark [ocean n]
              (cond
                (> n num-sharks) ocean
                (> (rand) 0.5) (cons (ref (make-shark n)) (add-shark ocean (inc n)))
                :else (cons (ref '()) (add-shark ocean n))))]
      (add-shark [] 1)))

(defn get-living-sharks
  "Returns a sequence of sharks that are alive"
  [ocean]
  (for 
    [shark-ref ocean :when (and (not-empty @shark-ref) (not (dead? @shark-ref)))]
    shark-ref))

(defn print-ocean
  "Prints out a list containing sharks and empty locations."
  [ocean]
  (println (map str-shark (for [shark ocean] @shark))))
  
;-----------------------------------------------------------------------------
; Define the interactions between a shark (as a ref to a struct) and the ocean
;-----------------------------------------------------------------------------

(defn get-shark
  "Returns a reference to the shark with the given id"
  [ocean id]
  (first
    (filter 
      (fn [shark] (and (not-empty @shark) (= (@shark :id) id))) 
      ocean)))

(defn location
  "Returns the current location (index) in the ocean of the given shark."
  ([ocean shark-ref]
    (location ocean shark-ref 0))
  ([ocean shark-ref n]
    (cond
      (empty? ocean) false 
      (= shark-ref (first ocean)) n
      :else (recur (rest ocean) shark-ref (inc n)))))

(defn location-next
  "Returns the next location (index) where the given shark will move. 
   Locations wrap around the edges."
  [ocean shark-ref]
  (let [curr-loc (location ocean shark-ref)]
    (cond
      (and (swimming-left? @shark-ref) (= curr-loc 0)) (dec (count ocean))
      (swimming-left? @shark-ref) (dec curr-loc)
      (and (swimming-right? @shark-ref) (= curr-loc (dec (count ocean)))) 0
      (swimming-right? @shark-ref) (inc curr-loc))))

(defn find-food
  "Returns a ref to the contents of the spot the shark is swimming towards."
  [ocean shark-ref]
  (nth ocean (location-next ocean shark-ref)))

;-----------------------------------------------------------------------------
; Functions that act on a shark's thread
;-----------------------------------------------------------------------------

(defn feed-and-swim
  "Triggers a shark to eat and swim and updates the ocean accordingly. 
   Since this is called by an agent, it returns a reference to the shark."
  [shark-ref ocean]
  (dosync
    (cond
      ; If the shark no longer exists or is dead, it doesn't do anything
      (or (empty? @shark-ref) (dead? @shark-ref))
      (do
        shark-ref)
      ; If the shark is too hungry, it dies
      (starved? @shark-ref) 
      (do
        (ref-set shark-ref (die @shark-ref))
        shark-ref)
      ; If the food space is empty, the shark moves and gets hungrier
      (empty? (deref (find-food ocean shark-ref))) 
      (let [food-ref (find-food ocean shark-ref)]
        (do
          (ref-set food-ref (make-hungrier @shark-ref))
          (ref-set shark-ref '())
          food-ref))
      ; Sharks can eat dead food or food smaller than themselves
      (can-eat? @shark-ref (deref (find-food ocean shark-ref))) 
      (let [food-ref (find-food ocean shark-ref)]
        (do
          (ref-set food-ref (eat @shark-ref @food-ref))
          (ref-set shark-ref '())
          food-ref))
      ; If this shark can't eat or swim right now, just return its reference
      :else 
      (do
        shark-ref))))

(defn pause
  "Sleeps an agent shark for anywhere between 1/2 to 1 second."
  [shark-ref]
  (do
    (. Thread (sleep (* (+ (rand 0.5) 0.5) 1000)))
    shark-ref))

(defn print-ocean-threaded
  "Queues a print request in the shark's thread to print the state of the ocean."
  [shark-ref ocean]
  (do
    (print-ocean ocean)
    shark-ref))

(defn print-shark-threaded
  "Queues a print request in the shark's thread to print the shark's status."
  [shark-ref]
  (do
    (cond
      (empty? @shark-ref) (println "Shark" (get-id @shark-ref) "no longer exists.")
      (starved? @shark-ref) (println "Shark" (get-id @shark-ref) "is starved.")
      (dead? @shark-ref) (println "Shark" (get-id @shark-ref) "is dead.")
      :else (println "Shark" (get-id @shark-ref) "has a hunger of" (get-hunger @shark-ref)))
    shark-ref))

;-----------------------------------------------------------------------------
; Get the whole thing rolling by calling (start [num-sharks])
;-----------------------------------------------------------------------------

(defn create-agent
  "Sets up an agent with error-handling for a shark."
  [shark-ref]
  (let [a (agent shark-ref)]
    (set-error-handler! 
      a 
      (fn [a-err exception] 
        (and        
          (println exception) 
          (clear-agent-errors a-err))))
    a))

(defn start 
  "Main function to get the simulation started."
  [num-sharks]
  (let [ocean (make-ocean num-sharks)]
    ; Run each shark from its own thread
    (let [agent-list 
          (take num-sharks (for [s (get-living-sharks ocean)] (create-agent s)))]
      (letfn [(run-agents []
                (if (> (count (get-living-sharks ocean)) 0)
                  (do
                    ; Queue a series of actions for each existing shark
                    (doseq [a agent-list]
                      (if (not-empty (deref @a)) 
                        (do                           
                          (send a print-ocean-threaded ocean)
                          (send a print-shark-threaded)
                          (send a feed-and-swim ocean)
                          (send a pause))))
                    ; Sleep for 1/4 second to limit requests in the queue
                    (. Thread (sleep 250))
                    (recur))))]
        (run-agents)))))
