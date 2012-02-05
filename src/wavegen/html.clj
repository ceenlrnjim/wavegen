(ns wavegen.html
  (:use [wavegen.clj]))


; {:type :header :product-names [Astera InfoSphere ...]} -> <tr class="header"><td>Req</td>...@@product-names@@
; {:type :category1 :weight 46.7 (derived) :computed-scores [1.456 2.498 ...] }
; {:type :category2 :weight 89.2 :computed-scores [...]}
; {:type :reqt :text "abc" :criteria {...} :weight 2.6 (computed to relative) :computed-scores [[1 0.1] [0 0]}
;
; probably need to nest reqts and categories first then flatten

