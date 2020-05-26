(define-map events 
    ((event-id (buff 64)))                  ;; ID for the event
    ((total-amount uint)                    ;; If this cap is not reached by the expiration date, event-passes will become refundable
     (expires-at uint)                      ;; Event expiration block height
     (exercised-at uint)                    ;; Event exercised block height (initially 0)
     (recipient principal)                  ;; If the group payment "succeeds", funds will be sent to this address
     (current-amount uint)))                ;; Keeping track - how much did we receive so far (initially 0)

(define-non-fungible-token event-pass uint)

(define-map event-pass-metadata 
    ((event-pass-id uint))                 ;; ID of the NFT
    ((event-id uint)                       ;; ID of the event
     (amount-spent uint)                   ;; Amount spent by the participant
     (refunded-at (optional uint))))       ;; Pass has already been refunded

;; create-event
(define-public (create-event (event-id (buff 64))
                             (total-amount uint)
                             (expires-blocks uint)      ;; number of blocks from creation to expiration
                             (recipient principal))
    (if (is-some (map-get? events ((event-id event-id))))
        (err "duplicate event")
        (let ((expires-at (+ block-height expires-blocks)))
             (map-insert events ((event-id event-id)) (tuple (total-amount total-amount) 
                                                             (expires-at expires-at)
                                                             (exercised-at (to-uint 0)) 
                                                             (recipient recipient)
                                                             (current-amount (to-uint 0))))
             (ok true))))

;; close-event-fundraise
(define-public (close-event-fundraise (event-id (buff 64)))
    (let ((event (unwrap! (map-get? events ((event-id event-id))) 
                          (err "event does not exist"))))
        (asserts! (< (get expires-at event) block-height) (err "too soon"))
        (asserts! (> (get exercised-at event) u0) (err "already exercised"))
        (asserts! (< (get current-amount event) (get total-amount event)) (err "not enough money yet"))
        (unwrap! 
            (stx-transfer? (get current-amount event) (as-contract tx-sender) (get recipient event))
            (err "unable to transfer stx"))
        (map-set events ((event-id event-id)) {
            total-amount: (get total-amount event),
            expires-at: (get expires-at event),
            recipient: (get recipient event),
            exercised-at: block-height,
            current-amount: u0
        })
        (ok true)))

;; buy-event-pass
(define-public (buy-event-pass (event-id uint) (amount uint)) 
    ;; Checks:
    ;; - ensure that event-id map to a valid event
    ;; - ensure that event.amount + amount <= upper-bound
    ;; - ensure that tx-sender.stx-balance >= amount
    ;; Mutations:
    ;; - transfer-stx from tx-sender to contract
    ;; - mint nft to tx-sender
    (ok true))

;; refund-event-pass
(define-public (refund-event-pass (event-pass-id uint))
    ;; Checks:
    ;; - ensure that caller is the owner of the pass
    ;; - ensure that event-id (from the nft-metadata) map to an event that is refundable
    ;; - ensure that current block > event.expiration-date
    ;; - ensure that pot.amount < upper-bound
    ;; Mutations:
    ;; - transfer-stx from contract to tx-sender
    ;; - update nft.refunded-at
    (ok true))