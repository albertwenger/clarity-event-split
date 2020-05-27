;; all the events
(define-map events 
    ((event-id (buff 64)))                  ;; ID for the event
    ((total-amount uint)                    ;; If this cap is not reached by the expiration date, event-passes will become refundable
     (expires-at uint)                      ;; Event expiration block height
     (funded-at uint)                       ;; Event funded block height (initially 0)
     (recipient principal)                  ;; If the group payment "succeeds", funds will be sent to this address
     (current-amount uint)))                ;; Keeping track - how much did we receive so far (initially 0)

;; issued when someone pays -- can get refunded if event is not fully funded
(define-non-fungible-token event-pass (buff 128))

;; all the event passes
(define-map event-passes 
    ((event-pass-id (buff 128)))            ;; ID of the NFT
    ((event-id (buff 64))                   ;; ID of the event
     (amount-spent uint)                    ;; Amount spent by the participant
     (refunded boolean)))                   ;; True if pass has been refunded

;; create-event
(define-public (create-event (event-id (buff 64))
                             (total-amount uint)
                             (expires-blocks uint)      ;; number of blocks from creation to expiration
                             (recipient principal))
    (if (is-some (map-get? events ((event-id event-id))))
        (err "duplicate event")
        (let ((expires-at (+ block-height expires-blocks)))
             (map-insert events ((event-id event-id)) {
                total-amount: total-amount, 
                expires-at: expires-at,
                funded-at: u0,
                recipient: recipient,
                current-amount: u0
             })
             (ok true))))

;; close-event-fundraise
(define-public (close-event-fundraise (event-id (buff 64)))
    (let ((event (unwrap! (map-get? events ((event-id event-id))) 
                          (err "event does not exist"))))
        (asserts! (< (get expires-at event) block-height) (err "too soon"))
        (asserts! (> (get funded-at event) u0) (err "event already funded"))
        (asserts! (< (get current-amount event) (get total-amount event)) (err "not enough money yet"))
        (unwrap! 
            (stx-transfer? (get current-amount event) (as-contract tx-sender) (get recipient event))
            (err "unable to transfer stx"))
        (map-set events ((event-id event-id)) {
            total-amount: (get total-amount event),
            expires-at: (get expires-at event),
            recipient: (get recipient event),
            funded-at: block-height,
            current-amount: u0
        })
        (ok true)))

;; buy-event-pass
(define-public (buy-event-pass (event-id (buff 64)) (amount uint))
    (let ((event (unwrap! (map-get? events ((event-id event-id))) 
                          (err "event does not exist")))
          (event-pass-id (concat event-id (concat "-" tx-sender))))  ;; can I create an event-pass-id this way?
        (asserts! (> (get funded-at event) u0) (err "event already funded"))
        (asserts! (> (stx-get-balance tx-sender) amount) (err "you have insufficient funds")) 
        (unwrap!
            (stx-transfer? amount tx-sender (as-contract tx-sender))   ;; is that how I transfer stacks to the contract?
            (err "unable to receive stx"))
        (map-insert event-passes ((event-pass-id event-pass-id)) {
            event-id: event-id,
            amount-spent: amount,
            refunded: false
        })
        (map-set events ((event-id event-id)) {
            total-amount: (get total-amount event),
            expires-at: (get expires-at event),
            recipient: (get recipient event),
            funded-at: (get funded-at event),
            current-amount: (+ (get current-amount event) amount)
        })
        (unwrap! 
            ((nft-mint? event-pass event-pass-id tx-sender)) 
            (err "unable to issue event pass"))
        (ok true)))

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