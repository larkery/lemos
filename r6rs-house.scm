;; ** The house and its parts

#|
Much of the input to the calculator is a description of a house.

This is done here using so-called record types; the define-record-type
syntax creates a new record type which has particular named fields,
and provides procedures for creating and accessing these records.

For all of this, the important point is that there are various
functions defined to access the different bits of the house.

These functions are used by the bredem-2012.scm implementation detail.

If you wanted to use bredem-2012.scm with another house description
that would work fine so long as you implemented equivalent functions.
|#

(define-record-type element
  (fields
   (immutable area)))

(define-record-type window
  (parent 'element)
  (fields
   (immutable gl)
   (immutable fr)
   (immutable zl)))

(define-record-type wall
  (parent 'element)
  (fields
   (immutable type)
   (immutable internal-insulation)
   (immutable external-insulation)
   (immutable cavity-insulation)))

(define-record-type bridge
  (fields
   (immutable length)
   (immutable psi)))

(define-record-type fabric
  (fields
   (immutable build-year)
   (immutable site-exposure)
   (immutable exposed-sides)



   ;; derived features
   (immutable floor-area)
   (immutable volume)                   ;AKA V_T (p16)
   (immutable Q50)                      ;measured air permeability
   (immutable storeys)                  ;number of storeys

   ;; fabric elements
   (immutable elements)

   ;; can be #f if we don't know anything
   (immutable bridges)

   ;; items from table 20 (counts)
   (immutable chimneys)
   (immutable open-flues)
   (immutable intermittent-fans)
   (immutable passive-vents)
   (immutable flueless-gas-fires)

   (ventilation-type)                   ; natural |
                                        ; positive-from-loft |
                                        ; mechanical-no-hr |
                                        ; mechanical-with-hr |
                                        ; mechanical-extract |
                                        ; positive-from-outside
   )
  )

(define-record-type dhw-system
  (fields

   )
  )

(define-record-type heating-system
  (fields

   )
  )

(define-record-type house
  (fields
   (immutable heating-system)
   (immutable dhw-system)
   (immutable fabric)
   )
  )
