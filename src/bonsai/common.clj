(ns bonsai.common
  (:require [quil.core :as q]
            [quip.sprite :as qpsprite]
            [quip.utils :as qpu]))

(def f qpu/hex->rgb)

(def dark-green [34 49 39])
(def leaf-green [24 152 67])
(def light-leaf-green [85 220 130])
(def light-blossom-pink [249 134 182])
(def blossom-pink-1 [249 134 182])
(def blossom-pink-2 [231 111 161])
(def blossom-pink-3 [198 73 125])
(def blossom-pink-4 [164 43 93])
(def birch-orange [232 152 28])
(def birch-yellow [244 198 53])
(def acer-red [210 45 24])
(def dark-acer-red [193 25 4])
(def acer-orange [214 71 19])
(def brown [95 50 42])
(def dark-brown [68 37 31])
(def sky-blue [57 194 205])
(def light-sky-blue [119 220 205])
(def white-blue [191 240 229])
(def white-green [219 250 229])
(def ceramic-white [255 253 216])
(def dark-ceramic-white [235 220 188])
(def slate-grey [155 173 183])
(def dark-slate-grey [113 126 133])

(defn fade-to-white
  [state progress maximum]
  (q/fill 255 (int (* 50 (/ progress maximum))))
  (q/rect 0 0 (q/width) (q/height)))

(defn get-branches
  [{:keys [current-scene] :as state}]
  (filter (qpsprite/group-pred :branches) (get-in state [:scenes current-scene :sprites])))
