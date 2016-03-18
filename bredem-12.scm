(use-modules (srfi srfi-1)
             (srfi srfi-8)
             (srfi srfi-9))

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

(define (distribute-monthly amount)
  "Given AMOUNT, spread it out into 12 parts weighted by the number of
days in each of the 12 months."
  (map (lambda (month)
         (* amount (/ (cadr month) 365.0)))
       months))

;; each part of this should directly map to BREDEM 2012
;; md5sum:
;; 10b15c890525dcb0557a0a57c7d5b8a1  BREDEM-2012-specification.pdf

;; * 1 - appliances and lighting
;; ** Seasonal wave

#|
Appliances and lighting both have an adjustment produced
by the following cosine function
|#

(define (seasonal-wave amplitude phase)
  (map
   (lambda (m)
     (let ([num (car m)]
           [days (cadr m)])
       (* (/ days 365.0)
          (+ 1 (* amplitude
                  (cos
                   (* 2 pi
                      (/ (- num phase) 12)))))))
     )
   months
   ))

(define Ea-monthly (seasonal-wave 0.157 1.78))
(define Elm-monthly (seasonal-wave 0.5 0.2))

;; ** Occupancy estimation

(define (estimated-occupancy TFA)
  "Estimate occupancy based on floor area (item A, page 6)"
  (if (> TFA 13.9)
      ;; if large area
      (let ([TFA' (- TFA 139.9)])
        (+ 1
           (* 1.76 (- 1 (exp (* -0.000349 (* TFA' TFA')))))
           (* 0.0013 TFA')))
      ;; otherwise
      1))

;; ** Lighting energy consumption (p6)
;; *** Definition of windows

(define-record-type window
  (make-window area gl fr zl)
  window?
  (area window-are)
  (gl window-gl)
  (fr window-fr)
  (zl window-zl))

(define (table-1 glazing)
  "Table 1, as a function. Page 6.
Given GLAZING (single|double|triple), produce a light transmission factor."
  (case glazing
    ((single) 0.9)
    ((double) 0.8)
    ((triple) 0.7)))

(define (table-2 frame)
  "Table 2, page 7.
Given FRAME (wood | metal | upvc), produce a frame factor."
  (case frame
    ((wood) 0.7)
    ((metal) 0.8)
    ((upvc) 0.7)))

(define (table-3 overshading)
  "Table 3, page 7.
Given OVERSHADING of (heavy | above-average | average |
below-average), produce a light access factor."
  (case overshading
    ((heavy) 0.5)
    ((above-average) 0.67)
    ((average) 0.83)
    ((below-average) 1)))

(define (make-standard-window area glazing frame overshading)
  "Create a window using the standard figures from tables 1 2 and 3.
This should be used when producing windows to be considered in the
lighting requirement."
  (make-window area
               (table-1 glazing)
               (table-2 frame)
               (table-3 overshading)))

;; *** Lighting requirement

(define (lighting-requirement TFA N L% windows)
  "Compute the monthly lighting requirement (item G, page 6).
TFA is the total floor area, and N the occupancy.
L% is the percentage low-energy lighting.
windows is a list of objects representing windows."
  (let* (;; B
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
         [ElT (* Eb C1 C2)])
    ;; G (monthly distribution)
    ;; this is just a matter of multiplying
    ;; each precomputed monthly value by the
    ;; monthly typical value in step F

    ;; the only step we are not doing here is the adding up to produce El
    (map (lambda (x) (* x ElT)) Elm-monthly)))

;; ** Appliance, pump, and fan energy consumption

(define (appliance-requirement TFA N)
  "Compute the monthly appliance requirement (item J), page 6
TFA is the total floor area and N the occupancy."
  (let* (;;I
         [Ea (* 184.4 (expt (* TFA N) 0.4174))])
    ;; J
    (map (lambda (x) (* x Ea)) Ea-monthly)))

(define (pump-and-fan-requirement ...)
  "Haven't done this yet.
It requires table 4, which is a lot of bits that we need to get straight."
  undefined)

;; ** Cooking energy consumption

(define (table-5 cooking-type)
  (case cooking-type
    ;type              Ec1a  Ec1b Ec2a Ec2b
    ((normal-electric) (values 275 55   0    0))
    ((normal-other)    (values 481 96   0    0))
    ((normal-dual)     (values 138 28   241  48))
    ((large-electric)  (values 361 78   0    0))
    ((large-other)     (values 631 136  0    0))
    ((large-dual)      (values 181  39   316  68))))

(define (range-power-consumption input)
  "Range power consumption, with defaults.
Unnamed table on page 5, row near the end.
Given INPUT of a number, use that number.
Given INPUT equal to electric, use 1500 watts.
Otherwise 2000 watts."
  (cond
   ((number? input) input)
   ((eq? input 'electric) 1500)
   (#t 2000)))

(define (cooking-requirement N cooking-type range-power-or-type)
  "Cooking requirement, defined on page 6.
N is the occupancy, cooking-type per `table-5', and
range-power-or-type is either electric, another fuel, or a number in watts"
  (receive
      [EC1A EC1B EC2A EC2B]
      (table-5 cooking-type)
    (let* (;; M:
           [EC1 (+ EC1A (* EC1B N))]
           [EC2 (+ EC2A (* EC2B N))]
           ;; N:
           [EC1m (distribute-monthly EC1)]
           [EC2m (distribute-monthly EC2)]
           ;; O: (pointwise addition, thanks scheme)
           [ECm (map + EC1m EC2m)]
           ;; P
           [Pr (range-power-consumption range-power-or-type)]
           ;; TODO: there is no clamp to zero in this.
           [ERm (map - (distribute-monthly (* Pr 0.024 365.0)) ECm)]
           ;; but wait - is the range off in June July August and September?
           )
      ;; thinking about it, these are not /per fuel/, which makes
      ;; emissions a bit of a nuisance.
      ;; may need to tidy that up later.
      (map + ECm ERm)
      )))

;; * 2- Energy required to heat water (p8)
;; ** 2.1 - The volume and energy content of heated water (p8)

(define (table-6 shower-type)
  "Table 6, page 8.
Hot water use per shower (litres), by type of shower"
  (case shower-type
    ((mixer) 28.8)
    ((combi-mixer) 44.4)
    ((pumped) 43.5)
    ((electric) 0)
    ((unknown) 18.7)
    (else 0)))

#|
Table 7 (page 8)
Hot water usage factor, by month
Just a constant.
|#
(define table-7
  '(1.1 1.06 1.02 0.98 0.94 0.9 0.9 0.94 0.98 1.02 1.06 1.1))

#|
Table 8 (page 9)
Monthly rise in temperature required for hot water
|#
(define table-8
  '(41.2 41.4 40.1 37.6 36.4 33.9 30.4 33.4 33.5 36.3 39.4 39.9))

(define (hot-water-requirement
         N
         shower-type
         showers-per-day
         baths-per-day
         volume-per-shower)
  "Produces the hot water requirement in terms of energy per month.
N is the occupancy.
shower-type is #f for no shower, or per table-6 above.
showers-per-day is #f or number of showers per day.
baths-per-day is #f or num. baths per day.
volume-per-shower is #f or the volume per shower."
  (let (;; A
        [Nshower (or showers-per-day (+ 0.65 (* 0.45 N)))]
        [Vps (or volume-per-shower (table-6 shower-type))]
        ;; B
        [Vd,shower (* Nshower Vps)]
        ;; C
        [Nbath (or baths-per-day
                   (if shower-type
                       (+ 0.19 (* 0.13 N))
                       (+ 0.5  (* 0.35 N))))]
        ;; D
        [Vd,bath (* Nbath 50.8)]
        ;; E
        [Vd,other (+ (* 9.8 N) 14)]
        ;; F
        [Vd,ave (+ Vd,shower Vd,bath Vd,other)]
        ;; G
        [Vd,m (map (lambda (x) (* x Vd,ave)) table-7)])
    ;; H
    (map (lambda (month Vd deltaT)
           (/ (* 4.18 Vd (cadr month) deltaT) 3600))
         months
         Vd,m
         table-8)
    ))

;; ** 2.2 - Water heating system losses (p9)

;; * Entire calculation

(define (bredem-12 ...)
  (let* ([N (or N (estimated-occupancy TFA))]
         [Elm (lighting-requirement TFA N L% windows)]
         [Eam (appliance-requirement TFA N)]
         [Epf (pump-and-fan-requirement ???)]
         )


    )
  )
