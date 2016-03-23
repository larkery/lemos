#|
This single file contains a monolithic implementation of BREDEM 2012
The specification document used should be in the repository

For total unambiguous traceability, here is its checksum:

file name: BREDEM-2012-specification.pdf
   md5sum: 10b15c890525dcb0557a0a57c7d5b8a1

If you can edit this using emacs, then outline-minor-mode and outshine
will help. The code is almost exactly mappable to the table of
contents of the BREDEM document, except where the document contains
forward references.

The code is written in scheme, a programming language designed for
teaching. Scheme's basic syntax is quite similar to that for the NHM;
however, the meaning of particular procedures is somewhat different.
|#

(use-modules ((rnrs) :version (6))
             (srfi srfi-26))

;; * 0 - Some definitions

;; ** The calendar

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

(define (month-number month) (car month))
(define (month-days month)   (cadr month))
(define (month-name month)   (caddr month))

(define pi 3.14159265358979323846264338328)

(define (distribute-monthly amount)
  "Given AMOUNT, spread it out into 12 parts weighted by the number of
days in each of the 12 months."
  (map (lambda (month)
         (* amount (/ (cadr month) 365.0)))
       months))

;; * 1 - appliances and lighting
;; ** Seasonal wave

#|
Appliances and lighting both have an adjustment produced
by the following cosine function
|#

(define (seasonal-wave amplitude phase)
  (map
   (lambda (m)
     (* (/ (month-days m) 365.0)
        (+ 1 (* amplitude
                (cos
                 (* 2 pi
                    (/ (- (month-number m) phase) 12))))))
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
                          (element-area window)
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
    (map (cut * <> Ea) Ea-monthly)))

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

;; * 2 - Energy required to heat water (p8)

;; ** 2.0 - System description

;; TODO define record type is annoying - make shorter one with syntax rule.
(define-record-type water-heating-system
  (make-water-heating-system
   type                                 ; broadly what type is it
   store-type                           ; what kind of store does it have
   store-volume
   store-insulation                     ; what kind of insulation does it have
   store-insulation-thickness           ; how much
   declared-loss
   cylinder-stat
   heat-timer
   store-timer
   store-in-cupboard
   keep-hot-facility
   ))

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
  (let* (;; A
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
    ;; H - convert vd into a monthly figure; this is the daily amount
    ;; times number of days in the month times delta t for the month
    ;; converted to kWh
    (map (lambda (month Vd deltaT)
           (/ (* 4.18 Vd (month-days month) deltaT) 3600))
         months
         Vd,m
         table-8)
    ))

;; ** 2.2 - Water heating system losses (p9)

(define (table-9
         type-of-storage
         type-of-system
         Vc                             ; store volume
         declared-loss
         cylinder-thermostat
         hot-water-timer
         store-timer
         airing-cupboard)
  "page 10; storage temperature factor.
Naturally this is not a table, but an algorithm.
No idea why it goes in a table.

The table contains NA in the two marked rows; this function
returns 1 in these cases.
"
  (if (community-system? type-of-system)
      1.0
      (case type-of-storage
        ((cylinder-immersion) 0.6)

        ((combi-close-coupled cylinder-indirect)
         (* 0.6
            ;; a) multiply by 1.3 if no cylinder stat
            (if cylinder-thermostat 1 1.3)
            ;; b) multiply by 0.9 if separate timer
            (if hot-water-timer 0.9 1)))

        ((combi-primary)
         (if declared-loss
             1.0 ;NA
             (+ 2.54 (* 0.00682 (max 0 (- 115 Vc))))))

        ((combi-secondary)
         (if declared-loss
             1.0 ;NA
             (+ 1.68 (* 0.00496 (max 0 (- 115 Vc))))))

        ((hot-water-only-thermal-store integrated-store-gas-fire-cpsu)
         (* (if declared-loss 0.89 1.08)

            ;; c) multiply by 0.81 if thermal store has separate tiimer
            (if store-timer 0.81 1)
            ;; d) multiply by 1.1 if not in airing cupboard
            (if airing-cupboard 1 1.1)))

        ((electric-cpsu)                   ; NOTE using default of 85 deg.
         (if declared-loss 1.09 1.0))
        )))

(define (table-10 pipework-insulation type-of-system)
  "p10 - table 10 - primary pipework insulated"
  (if (community-system? type-of-system)
      1.0                               ; this is a footnote to table 11. good work someone.
      (case pipework-insulation
        ((uninsulated) 0.0)
        ((first-meter) 0.1)
        ((all-accessible) 0.3)
        ((fully-insulated) 1.0))))

(define (table-11 cylinder-stat separately-timed type-of-system)
  "p10 - table 11- hours per day PP is hot"
  (cond
   ;;                                    *  *  *  *
   ;;                     J  F  M  A  M  J  J  A  S  O  N  D
   ((community-system? type-of-system)
                        '(3  3  3  3  3  3  3  3  3  3  3  3))
   ((not cylinder-stat) '(11 11 11 11 11 3  3  3  3  11 11 11))
   (separately-timed    '(5  5  5  5  5  3  3  3  3  5  5  5))
   (t                   '(3  3  3  3  3  3  3  3  3  3  3  3))))

(define (table-12 solar-water-heating)
  "p10 - table 12 - primary circuit loss adjustment factors with solar water heating"
  (if solar-water-heating
      (make-list 12 1.0)
      '(1.0 1.0 0.94 0.7 0.45 0.44 0.44 0.48 0.76 0.94 1.0 1.0)))

;; *** TODO table 13 is missing

(define (table-13)
  "p11 - table 13 - combination boiler loss equations
Why is this a table? it's not a table.
To start with it's going to be twelve zeroes."
  ;; TODO handle type of system properly
  (make-list 12 0.0))

(define (hot-water-system-losses
         QHW,m                          ; monthly energy content of
                                        ; hot water, per
                                        ; hot-water-requirement above

         central-water-heating          ; #t or #f
         Qst,man                        ; manuf. declared loss or #f
         STF                            ; storage temp factor or #f

         ;; inputs needed for table 9, if stf omitted:
         type-of-system                 ; a type of heating system
         type-of-storage
         Vc
         cylinder-thermostat
         hot-water-timer
         store-timer
         airing-cupboard

         cylinder-insulation-type
         t                              ; cylinder insulation thickness

         solar-water-heating            ; do we have solar water heating
         )
  "p9
QHW,m should be computed by using hot-water-requirement."

  (let* (;; compute storage-temperature-factor if omitted
         ;; using table 9
         [STF (or STF (table-9
                       type-of-storage
                       type-of-system
                       Vc
                       Qst,man
                       cylinder-thermostat
                       hot-water-timer
                       store-timer
                       airing-cupboard))]

         ;; compute solar adjustment factor
         [fpa,m (if (and cylinder-thermostat
                         solar-water-heating)
                    table-12
                    (make-list 12 1.0))]

         ;; compute hours per day primary hot
         [hpp,m (table-11 cylinder-thermostat
                          hot-water-timed
                          type-of-system)]

         ;; compute fraction of primary pipework insulated
         [fpp (table-10 pipework-insulation type-of-system)]

         ;; A
         [QD,m (if central-water-heating
                   (* 0.15 QHW,m)
                   0)]
         ;; B
         [Qst,d (if Qst,man
                    (* Qst,man STF)
                    (let (;; a.
                          [L (case cylinder-insulation-type
                               ((mineral-wool) (+ 0.005 (/ 1.76 (+ t 12.8))))
                               ((factory-foam) (+ 0.005 (/ 0.55 (+ t 4.0))))
                               ((electric-cpsu) 0.022))]
                          ;; b.
                          [Vf (expt (/ 120 Vc) 0.3333)])
                      ;; c.
                      (* L Vc Vf STF)))]
         ;; C
         [Qst,m (map (lambda (m) (* Qst,d (month-days m))) months)]

         ;; D
         [QP,m (map (lambda (m fpa hpp)
                      (* (month-days m)
                         14.0
                         fpa
                         (* hpp
                            (+ 0.0263
                               (+ (* 0.0091 fpp)
                                  (* 0.0245 (- 1 fpp)))))
                         ))
                    months fpa,m hpp,m)]
         ;;E
         [Qcom,m (table-13 type-of-system)])
    (values QP,m Qcom,m)))

;; ** TODO 2.3 - Energy required for electric showers (p11)
;; ** TODO 2.4 - Hot water from solar water heating systems
;; *** TODO 2.4.1 - Calculating the solar energy incident on a solar collector
;; *** TODO 2.4.2 - Calculating the heat output of a solar water heater
;; ** TODO 2.5 - Net water heating energy requirement
;; * TODO 3 - Heat Loss

(define (fabric-heat-loss
         fabric
         element-u-value   ; a function to get the u-value of an
                           ; element, e.g. a sap u-values function.
         y                    ; a thermal bridge factor, or #f and we
                              ; will guess per footnote on p16
         )
  "p17 - fabric heat loss"
  (let* (;; p16 footnote
         [build-year (fabric-build-year fabric)]
         [thermal-bridges (fabric-thermal-bridges fabric)]
         [external-elements (fabric-elements fabric)]

         ;; determine y if not supplied
         [y (or y (cond
                   ((< build-year 2002) 0.15)
                   ((< build-year 2006) 0.11)
                   (t 0.08)))]
         ;; A.
         [Htb (if thermal-bridges
                  (apply + (map (lambda (bridge)
                                  (* (bridge-length bridge)
                                     (bridge-psi bridge)))
                                thermal-bridges))
                  (* y (apply + (map element-area external-elements))))]
         [surface-loss (apply + (map (lambda (element)
                                       (* (element-area element)
                                          (element-u-value element)))
                                        ; the u-value adjustment for
                                        ; curtains is found within
                                        ; element-u-value
                                     external-elements))])
    ;; B.
    (+ surface-loss Htb)))

(define (table-19 fabric)
  "p18 - table 19 - fabric structural infiltration"
  ;; take an area-weighted average where more than one category applies
  (+ (* (fabric-storeys fabric) 0.1)    ; stack effect
     ;; TODO area weighted average for walls
     ;; TODO floor type (not sure what that really means)
     ;; TODO unsealed loft hatch
     ;; windows and doors, area weighted avg.
     ;; draught lobby on main door.
   )
  )

(define (table-20 fabric)
  "p18 - table 20 - a function of building fabric which calculates ventilation in m3/hr"
  (+ (* 40.0 (fabric-chimneys fabric))
     (* 20.0 (fabric-open-flues fabric))
     (* 10.0 (fabric-intermittent-fans fabric))
     (* 10.0 (fabric-passive-vents fabric))
     (* 40.0 (fabric-flueless-gas-fires fabric))))

(define (table-21 fabric)
  "p18 - table 21 - site exposure factor"
  (case (fabric-site-exposure fabric)
    ((exposed) 1.1)
    ((above-average) 1.05)
    ((average) 1)
    ((below-average) 0.95)
    ((sheltered) 0.9)))

(define (table-22 fabric)
  "p19 - table 22 - dwelling exposure factor"
  (case (fabric-exposed-sides fabric)
    ((4) 1)
    ((3) 0.925)
    ((2) 0.85)
    ((1) 0.775)
    ((0) 0.7)))

(define (ventilation-heat-loss
         fabric
         wind-speed)          ;vreg,m on p16 - really table A3, appendix A
  "p17 - ventilation heat loss"
  (let* ([Fdv (table-20 fabric)]
         ;; C.
         [Vt (fabric-volume fabric)]
         [Ldv (/ Fdv Vt)]
         ;; D.
         [Q50 (fabric-Q50 fabric)]
         [Lfab (if Q50
                   (/ Q50 20)
                   (table-19 fabric))]
         ;; E.
         [She (table-21 fabric)]                          ;site exposure factor
         [Shd (table-22 fabric)]                          ;dwelling exposure factor
         [mystery-factor (/ (* (+ Lfab Ldv) She Shd) 4)]
         [Lsub,m (map (lambda (vreg) (* vreg mystery-factor)) wind-speed)]
         ;; F.
         [Lv,m
          (map
           (case (fabric-ventilation-type)
            ((natural positive-from-loft)
             (lambda (Lsub) (if (>= Lsub 1)
                                Lsub
                                (+ 0.5 (/ (expt Lsub 2) 2)))))

            ((mechanical-no-hr)
             (lambda (Lsub) (+ 0.5 Lsub)))

            ((mechanical-with-hr)
             ;; ASSUMPTION: no test data for MVHR available; using
             ;; default efficiency of 60% which goes into the 0.4
             ;; below (it is (1 - 60%))
             (lambda (Lsub) (+ Lsub (* 0.5 0.4))))

            ((mechanical-extract positive-from-outside)
             (lambda (Lsub) (if (< Lsub 0.25) 0.5 (+ Lsub 0.25)))))

           Lsub,m)])
    ;; G is the output of this part
    (map (lambda (Lv) (* 0.33 Lv Vt)) Lv,m)))

(define (heat-losses fabric element-u-value y wind-speed)
  "p17 - Heat transfer coefficients and heat loss parameter.
Returns H,m, HLP,m and H3."
  (let* ([Hf   (fabric-heat-loss fabric element-u-value y)]
         [Hv,m (ventilation-heat-loss fabric wind-speed)]
         ;; add them up per month
         ;; H.
         [H,m (map + Hv,m (make-list 12 Hf))]
         ;; I.
         [TFA (fabric-floor-area fabric)]
         [HLP,m (map (lambda (H) (/ H TFA)))]
         ;; J.
         ;; TODO all kinds of waffle about zones and stairs
         [H3 1])
    ;; this part returns multiple values
    (values H,m HLP,m H3)
    ))

;; * TODO 4 - Thermal mass parameter
;; * TODO 5 - Solar heat gain
;; * TODO 6 - Internal heat gain and total heat gain
;; * TODO 7 - Mean internal temperature
;; * TODO 8 - Space heating energy requirement
;; * TODO 9 - Cooling energy requirement
;; * TODO 10 - PV and wind turbines
;; * TODO 11 - Combining outputs
