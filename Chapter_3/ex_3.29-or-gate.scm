(define (or-gate a1 a2 output)
  (let ((wire1 (make-wire))
	(wire2 (make-wire))
	(wire3 (make-wire)))
    (inverter a1 wire1)
    (inverter a2 wire2)
    (and-gate wire1 wire2 wire3)
    (inverter wire3 output)))