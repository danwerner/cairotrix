(ns org.segfaulted.cairotris.core
  "Cairotris - a Pajitnovian falling blocks game built on top of Clojure, GTK+ and Cairo"
  (:gen-class :name org.segfaulted.cairotris.core :main true )
  (:require [clojure.string :as str])
  (:import [org.gnome.gtk Gtk Window Window$DeleteEvent VBox DrawingArea 
                          Widget$ExposeEvent]
           [org.gnome.gdk Event EventExpose]
           [org.freedesktop.cairo Context]))

(defonce gtk-init (Gtk/init (make-array String 0)))

(def MAX-FPS 10)
(def MIN-MILLIS-PER-FRAME (/ 1000 MAX-FPS))
(def TURN-TIME 1000)

(def WORLD-HEIGHT 15) ;blocks
(def WORLD-WIDTH 9)
(def BLOCK-SIZE 40) ;px
(def STARTING-POS [(dec (quot WORLD-WIDTH 2)) 0])

(def WORLD-WIN-HEIGHT (* WORLD-WIDTH BLOCK-SIZE)) ;px
(def WORLD-WIN-WIDTH (* WORLD-HEIGHT BLOCK-SIZE))

(def BACKGROUND-COLOR [0.0 0.0 0.0])

(def RUNNING (atom true))

(def COLORS
  {:black [0.0 0.0 0.0]
   :white [1.0 1.0 1.0]
   :red   [1.0 0.0 0.0]
   :green [0.0 1.0 0.0]
   :blue  [0.0 0.0 1.0]})

(def TETRACOLORS (dissoc COLORS :black))

;;; Util

(defn set-source-rgb [cr [r g b]]
  (.setSource cr r g b))

(defmacro with-context [cr & body]
  `(let [cr# ~cr]
     (try
       (.save cr#)
       (doto cr#
         ~@body)
       (finally
         (.restore cr#)))))

(defn indexed [coll]
  (map-indexed vector coll))

(defn map-values [f coll]
  (into {} (map (fn [[k v]] [k (f v)]) coll)))

(defn current-millis []
  (System/currentTimeMillis))

(defn debug [msg & args]
  (assert (or (instance? String msg) (not args)))
  (println (apply format (str msg) args)))

;;; Game Logic

(defn parse-tetra [tetra]
  (let [lines  (->> tetra str/split-lines (map str/trim))
        parsed (for [[y line] (indexed lines)
                    [x chr] (indexed line)]
                 (case chr
                   \X [x y]
                   \_ nil))]
    (set (keep identity parsed))))
  
(def TETRAS-RAW {
  :O "XX__
      XX__
      ____"
  :T "_X__
      XXX_
      ____"
})

(def TETRAS (map-values parse-tetra TETRAS-RAW))

(defn random-tetra []
  {:form (rand-nth (keys TETRAS))
   :color (rand-nth (keys TETRACOLORS))})

(defn make-game []
  {:turn 0
   :blocks #{}
   :current nil
   :next (random-tetra)
   :position nil})

(defn drop-new-tetra [game]
  (assoc game
    :current (:next game)
    :next (random-tetra)
    :position STARTING-POS))

(defn step-turn [game]
  ;; move tetra down:
  ;; calculate new positions of current tetra blocks
  ;; no collision => move them down
  ;; if collision would have occured, affix blocks and generate new tetra
  ;; play sound here? or in the game loop => separation of concerns?
  game)

;;; Drawing

(defn draw-bg [cr]
  (doto cr
    (set-source-rgb BACKGROUND-COLOR)
    (.paint)))

(defn draw-block [cr block]
  (let [{:keys [x y color]} block]
    (doto cr
      (set-source-rgb (COLORS color))
      (.rectangle (* x BLOCK-SIZE) (* y BLOCK-SIZE)
                  BLOCK-SIZE        BLOCK-SIZE)
      (.fill)
      (set-source-rgb BACKGROUND-COLOR)
      (.stroke))))

(defn draw-blocks [cr blocks]
  (doseq [block blocks]
    (draw-block cr block)))

(defn draw-tetra [cr tetra position]
  (let [{:keys [form color]}  tetra
        [tx ty]     position
        ;; Turning the current tetra into blocks with absolute world
        ;; coordinates allows us to use draw-blocks unchanged.
        rel-coords  (TETRAS form)
        blocks      (for [[x y] rel-coords]
                      {:x (+ x tx), :y (+ y ty), :color color})]
    (debug blocks)
    (draw-blocks cr blocks)))
  

(defn draw-all [^DrawingArea worldarea game]
  (let [target  (.getWindow worldarea)
        cr      (Context. target)
        {:keys [blocks current position]} game]
    (doto cr
      (draw-bg)
      (draw-blocks blocks)
      (draw-tetra current position))))

(defn redraw-all [^DrawingArea worldarea]
  (.queueDrawArea worldarea 0 0 WORLD-WIN-WIDTH WORLD-WIN-HEIGHT))


;;; Main

(defn game-loop [^DrawingArea worldarea game-ref]
  (let [game-start (current-millis)]
    (loop [game @game-ref]
      (let [turn-start (current-millis)
            turn       (quot (- turn-start game-start) TURN-TIME)
            new-turn?  (< (:turn game) turn)
            game       (assoc game :turn turn)
            ;; TODO: check for input
            game       (if new-turn? (step-turn game) game)
            turn-time  (- (current-millis) turn-start)
            wait-time  (max 0 (- MIN-MILLIS-PER-FRAME turn-time))]

        ;; Expose events render the current state of the world.
        (reset! game-ref game)
        (redraw-all worldarea)

        (Thread/sleep wait-time)
        (if @RUNNING
          (recur game))))))

(defn -main []
  (let [win        (Window.)
        box        (VBox. false 1)
        worldarea  (DrawingArea.)
        game-ref   (atom (drop-new-tetra (make-game)))]
    (.connect win
      (proxy [Window$DeleteEvent] []
        (onDeleteEvent [source event]
          (reset! RUNNING false)
          (Gtk/mainQuit)
          false)))
    (.connect worldarea
      (proxy [Widget$ExposeEvent] []
        (onExposeEvent [source event]
          (draw-all source @game-ref)
          true)))
    (doto box
      (.add worldarea))
    (doto win
      (.add box)
      (.setTitle "Cairotris")
      (.setDefaultSize WORLD-WIN-HEIGHT WORLD-WIN-WIDTH)
      (.showAll))
    (future (Gtk/main))
    (game-loop worldarea game-ref)
    ;; I don't use agents, so why is this necessary?
    (shutdown-agents)))
