(use-modules (srfi srfi-1))

(define months
  '((1 31 january)
    (2 28 february)
    (3 31 march)
    (4 30 april)
    (5 31 may)
    (6 30 june)
    (7 31 july)
    (8 31 august)
    (9 30 september)
    (10 31 october)
    (11 30 november)
    (12 31 december)))

(define pi 3.14159265358979323846264338328)

(define Elm-monthly
  (map
   (lambda (m)
     (* (/ (cadr m) 365.0)
        (+ 1 (* 0.5
                (cos
                 (* 2 pi
                    (/ (- (car m) 0.2) 12)))))))
   months
   ))

;; each part of this should directly map to BREDEM 2012
;; md5sum:
;; 10b15c890525dcb0557a0a57c7d5b8a1  BREDEM-2012-specification.pdf

;; * 1 - appliances and lighting
;; ** Lighting energy consumption (p6)

(define (estimated-occupancy TFA)
  "p9, equation A."
  (if (> TFA 13.9)
      ;; if large area
      (let ([TFA' (- TFA 139.9)])
        (+ 1
           (* 1.76 (- 1 (exp (* -0.000349 (* TFA' TFA')))))
           (* 0.0013 TFA')))
      ;; otherwise
      1))

(define (lighting-requirement TFA N L% windows)
  "p9, equations B through H, using A"
  (let* (;; A
         [N (or N (estimated-occupancy TFA))]
         ;; B
         [Eb (* 59.73 (expt (* TFA N) 0.4714))]
         ;; C
         [C1 (- 1 (* 0.5 L%))]
         ;; D
         [Gdl (/ (apply
                  + (map
                     (lambda (window)
                       (* 0.9
                          (window-area window)
                          (window-gl window)
                          (window-fr window)
                          (window-zl window)))
                     windows)) TFA)]
         ;; E
         [C2 (if (<= Gdl 0.095)
                 (+ (* 52.52 (expt Gdl 2))
                    (* -9.94 Gdl)
                    1.433)
                 0.96)]
         ;; F
         [ElT (* Eb C1 C2)]
         ;; G (monthly distribution)
         ;; this is just a matter of multiplying
         ;; each precomputed monthly value by the
         ;; monthly typical value in step F
         [Elm (map (lambda (x) (* x ElT)) Elm-monthly)])
    ;; finally step H: add them up
    (apply + Elm)))

;; ** Appliance, pump, and fan energy consumption

(define appliance-requirement (TFA N))

(define pump-and-fan-requirement (...))

;; ** Cooking energy consumption
;; * 2- Energy required to heat water
