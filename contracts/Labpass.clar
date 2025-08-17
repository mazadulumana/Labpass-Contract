
;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

;; token definitions
(define-non-fungible-token labpass uint)

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-LISTING-NOT-FOUND (err u102))
(define-constant ERR-WRONG-COMMISSION (err u103))
(define-constant ERR-LISTING-EXPIRED (err u104))
(define-constant ERR-NFT-NOT-FOUND (err u105))
(define-constant ERR-SENDER-EQUALS-RECIPIENT (err u106))
(define-constant ERR-PASS-EXPIRED (err u107))
(define-constant ERR-UNAUTHORIZED-ACCESS (err u108))
(define-constant ERR-INVALID-LAB (err u109))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u110))
(define-constant ERR-EQUIPMENT-NOT-FOUND (err u111))
(define-constant ERR-EQUIPMENT-UNAVAILABLE (err u112))
(define-constant ERR-RESERVATION-NOT-FOUND (err u113))
(define-constant ERR-RESERVATION-CONFLICT (err u114))
(define-constant ERR-INVALID-TIME-SLOT (err u115))
(define-constant ERR-EQUIPMENT-MAINTENANCE (err u116))
(define-constant ERR-RESERVATION-EXPIRED (err u117))
(define-constant ERR-EQUIPMENT-INACTIVE (err u118))
(define-constant ERR-CERTIFICATION-NOT-FOUND (err u119))
(define-constant ERR-CERTIFICATION-EXPIRED (err u120))
(define-constant ERR-INSUFFICIENT-CERTIFICATIONS (err u121))
(define-constant ERR-NOT-AUTHORIZED-INSTRUCTOR (err u122))
(define-constant ERR-CERTIFICATION-ALREADY-EXISTS (err u123))
(define-constant ERR-INVALID-CERTIFICATION (err u124))

;; data vars
(define-data-var last-token-id uint u0)
(define-data-var lab-count uint u0)
(define-data-var commission uint u250)
(define-data-var equipment-count uint u0)
(define-data-var reservation-count uint u0)
(define-data-var certification-count uint u0)

;; data maps
(define-map token-count principal uint)
(define-map market (tuple (token-id uint) (owner principal)) 
  (tuple (price uint) (commission uint)))

(define-map lab-passes uint 
  (tuple 
    (lab-id uint)
    (owner principal)
    (expiry-block uint)
    (access-level uint)
    (created-at uint)))

(define-map labs uint
  (tuple
    (name (string-ascii 64))
    (owner principal)
    (price-per-hour uint)
    (max-access-level uint)
    (active bool)))

(define-map lab-access-log 
  (tuple (lab-id uint) (pass-id uint) (user principal))
  (tuple (access-time uint) (duration uint)))

(define-map user-lab-history principal (list 100 uint))

(define-map lab-equipment uint
  (tuple
    (name (string-ascii 64))
    (lab-id uint)
    (hourly-rate uint)
    (available bool)
    (maintenance-start uint)
    (maintenance-end uint)
    (max-reservation-hours uint)
    (requires-training bool)))

(define-map equipment-reservations uint
  (tuple
    (equipment-id uint)
    (user principal)
    (pass-id uint)
    (start-time uint)
    (end-time uint)
    (total-cost uint)
    (status uint)
    (created-at uint)))

(define-map user-reservations principal (list 50 uint))

(define-map equipment-schedule 
  (tuple (equipment-id uint) (time-slot uint))
  (tuple (reserved bool) (reservation-id uint)))

(define-map safety-certifications uint
  (tuple
    (name (string-ascii 64))
    (description (string-ascii 256))
    (validity-months uint)
    (required-for-labs (list 20 uint))
    (required-for-equipment (list 20 uint))
    (created-by principal)
    (active bool)))

(define-map user-certifications 
  (tuple (user principal) (certification-id uint))
  (tuple
    (issued-at uint)
    (expires-at uint)
    (issued-by principal)
    (status uint)))

(define-map authorized-instructors principal (list 20 uint))

(define-map lab-certification-requirements uint (list 10 uint))

(define-map equipment-certification-requirements uint (list 10 uint))

(define-map user-certification-history principal (list 100 uint))

;; public functions
(define-public (create-lab (name (string-ascii 64)) (price-per-hour uint) (max-access-level uint))
  (let 
    (
      (lab-id (+ (var-get lab-count) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set labs lab-id 
      (tuple 
        (name name)
        (owner tx-sender)
        (price-per-hour price-per-hour)
        (max-access-level max-access-level)
        (active true)))
    (var-set lab-count lab-id)
    (ok lab-id)))

(define-public (mint-labpass (lab-id uint) (duration-hours uint) (access-level uint))
  (let 
    (
      (token-id (+ (var-get last-token-id) u1))
      (lab-info (unwrap! (map-get? labs lab-id) ERR-INVALID-LAB))
      (expiry-block (+ stacks-block-height (* duration-hours u144)))
      (total-cost (* (get price-per-hour lab-info) duration-hours))
    )
    (asserts! (get active lab-info) ERR-INVALID-LAB)
    (asserts! (<= access-level (get max-access-level lab-info)) ERR-UNAUTHORIZED-ACCESS)
    (try! (stx-transfer? total-cost tx-sender (get owner lab-info)))
    (try! (nft-mint? labpass token-id tx-sender))
    (map-set lab-passes token-id
      (tuple
        (lab-id lab-id)
        (owner tx-sender)
        (expiry-block expiry-block)
        (access-level access-level)
        (created-at stacks-block-height)))
    (var-set last-token-id token-id)
    (map-set token-count tx-sender (+ (get-balance tx-sender) u1))
    (ok token-id)))

(define-public (access-lab (pass-id uint) (lab-id uint))
  (let 
    (
      (pass-info (unwrap! (map-get? lab-passes pass-id) ERR-NFT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs lab-id) ERR-INVALID-LAB))
    )
    (asserts! (is-eq (get owner pass-info) tx-sender) ERR-NOT-TOKEN-OWNER)
    (asserts! (is-eq (get lab-id pass-info) lab-id) ERR-INVALID-LAB)
    (asserts! (< stacks-block-height (get expiry-block pass-info)) ERR-PASS-EXPIRED)
    (asserts! (get active lab-info) ERR-INVALID-LAB)
    (asserts! (has-required-certifications tx-sender lab-id) ERR-INSUFFICIENT-CERTIFICATIONS)
    (map-set lab-access-log 
      (tuple (lab-id lab-id) (pass-id pass-id) (user tx-sender))
      (tuple (access-time stacks-block-height) (duration u1)))
    (let 
      (
        (current-history (default-to (list) (map-get? user-lab-history tx-sender)))
      )
      (map-set user-lab-history tx-sender (unwrap! (as-max-len? (append current-history pass-id) u100) (ok true))))
    (ok true)))

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
    (asserts! (not (is-eq sender recipient)) ERR-SENDER-EQUALS-RECIPIENT)
    (let 
      (
        (pass-info (unwrap! (map-get? lab-passes token-id) ERR-NFT-NOT-FOUND))
      )
      (map-set lab-passes token-id (merge pass-info (tuple (owner recipient))))
      (try! (nft-transfer? labpass token-id sender recipient))
      (map-set token-count sender (- (get-balance sender) u1))
      (map-set token-count recipient (+ (get-balance recipient) u1))
      (ok true))))

(define-public (list-in-ustx (token-id uint) (price uint) (comm-trait <commission-trait>))
  (let 
    (
      (listing (tuple (token-id token-id) (owner tx-sender)))
    )
    (asserts! (is-owner token-id tx-sender) ERR-NOT-TOKEN-OWNER)
    (map-set market listing (tuple (price price) (commission (var-get commission))))
    (ok true)))

(define-public (unlist-in-ustx (token-id uint))
  (begin
    (asserts! (is-owner token-id tx-sender) ERR-NOT-TOKEN-OWNER)
    (map-delete market (tuple (token-id token-id) (owner tx-sender)))
    (ok true)))

(define-public (buy-in-ustx (token-id uint) (comm-trait <commission-trait>))
  (let 
    (
      (owner (unwrap! (nft-get-owner? labpass token-id) ERR-NFT-NOT-FOUND))
      (listing (unwrap! (map-get? market (tuple (token-id token-id) (owner owner))) ERR-LISTING-NOT-FOUND))
      (price (get price listing))
    )
    (try! (stx-transfer? price tx-sender owner))
    (try! (transfer token-id owner tx-sender))
    (map-delete market (tuple (token-id token-id) (owner owner)))
    (ok true)))

(define-public (set-lab-status (lab-id uint) (active bool))
  (let 
    (
      (lab-info (unwrap! (map-get? labs lab-id) ERR-INVALID-LAB))
    )
    (asserts! (is-eq tx-sender (get owner lab-info)) ERR-NOT-TOKEN-OWNER)
    (map-set labs lab-id (merge lab-info (tuple (active active))))
    (ok true)))

(define-public (extend-pass (pass-id uint) (additional-hours uint))
  (let 
    (
      (pass-info (unwrap! (map-get? lab-passes pass-id) ERR-NFT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs (get lab-id pass-info)) ERR-INVALID-LAB))
      (additional-cost (* (get price-per-hour lab-info) additional-hours))
      (new-expiry (+ (get expiry-block pass-info) (* additional-hours u144)))
    )
    (asserts! (is-eq (get owner pass-info) tx-sender) ERR-NOT-TOKEN-OWNER)
    (try! (stx-transfer? additional-cost tx-sender (get owner lab-info)))
    (map-set lab-passes pass-id (merge pass-info (tuple (expiry-block new-expiry))))
    (ok true)))

(define-public (add-equipment (lab-id uint) (name (string-ascii 64)) (hourly-rate uint) (max-hours uint) (requires-training bool))
  (let 
    (
      (equipment-id (+ (var-get equipment-count) u1))
      (lab-info (unwrap! (map-get? labs lab-id) ERR-INVALID-LAB))
    )
    (asserts! (is-eq tx-sender (get owner lab-info)) ERR-NOT-TOKEN-OWNER)
    (map-set lab-equipment equipment-id
      (tuple
        (name name)
        (lab-id lab-id)
        (hourly-rate hourly-rate)
        (available true)
        (maintenance-start u0)
        (maintenance-end u0)
        (max-reservation-hours max-hours)
        (requires-training requires-training)))
    (var-set equipment-count equipment-id)
    (ok equipment-id)))

(define-public (make-reservation (equipment-id uint) (pass-id uint) (start-time uint) (duration-hours uint))
  (let 
    (
      (reservation-id (+ (var-get reservation-count) u1))
      (equipment-info (unwrap! (map-get? lab-equipment equipment-id) ERR-EQUIPMENT-NOT-FOUND))
      (pass-info (unwrap! (map-get? lab-passes pass-id) ERR-NFT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs (get lab-id equipment-info)) ERR-INVALID-LAB))
      (end-time (+ start-time (* duration-hours u144)))
      (total-cost (* (get hourly-rate equipment-info) duration-hours))
    )
    (asserts! (is-eq (get owner pass-info) tx-sender) ERR-NOT-TOKEN-OWNER)
    (asserts! (is-eq (get lab-id pass-info) (get lab-id equipment-info)) ERR-INVALID-LAB)
    (asserts! (< stacks-block-height (get expiry-block pass-info)) ERR-PASS-EXPIRED)
    (asserts! (get available equipment-info) ERR-EQUIPMENT-UNAVAILABLE)
    (asserts! (not (is-in-maintenance equipment-id start-time end-time)) ERR-EQUIPMENT-MAINTENANCE)
    (asserts! (has-equipment-certifications tx-sender equipment-id) ERR-INSUFFICIENT-CERTIFICATIONS)
    (asserts! (<= duration-hours (get max-reservation-hours equipment-info)) ERR-INVALID-TIME-SLOT)
    (asserts! (> start-time stacks-block-height) ERR-INVALID-TIME-SLOT)
    (asserts! (is-time-slot-available equipment-id start-time end-time) ERR-RESERVATION-CONFLICT)
    (try! (stx-transfer? total-cost tx-sender (get owner lab-info)))
    (map-set equipment-reservations reservation-id
      (tuple
        (equipment-id equipment-id)
        (user tx-sender)
        (pass-id pass-id)
        (start-time start-time)
        (end-time end-time)
        (total-cost total-cost)
        (status u1)
        (created-at stacks-block-height)))
    (begin 
      (block-time-slots equipment-id start-time end-time reservation-id)
      (let 
        (
          (current-reservations (default-to (list) (map-get? user-reservations tx-sender)))
        )
        (map-set user-reservations tx-sender 
          (unwrap-panic (as-max-len? (append current-reservations reservation-id) u50)))))
    (var-set reservation-count reservation-id)
    (ok reservation-id)))

(define-public (cancel-reservation (reservation-id uint))
  (let 
    (
      (reservation-info (unwrap! (map-get? equipment-reservations reservation-id) ERR-RESERVATION-NOT-FOUND))
      (equipment-info (unwrap! (map-get? lab-equipment (get equipment-id reservation-info)) ERR-EQUIPMENT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs (get lab-id equipment-info)) ERR-INVALID-LAB))
      (refund-amount (if (> (get start-time reservation-info) (+ stacks-block-height u144))
                      (get total-cost reservation-info)
                      (/ (get total-cost reservation-info) u2)))
    )
    (asserts! (is-eq (get user reservation-info) tx-sender) ERR-NOT-TOKEN-OWNER)
    (asserts! (is-eq (get status reservation-info) u1) ERR-RESERVATION-EXPIRED)
    (asserts! (> (get start-time reservation-info) stacks-block-height) ERR-RESERVATION-EXPIRED)
    (try! (stx-transfer? refund-amount (get owner lab-info) tx-sender))
    (begin
      (map-set equipment-reservations reservation-id 
        (merge reservation-info (tuple (status u3))))
      (unblock-time-slots (get equipment-id reservation-info) 
                          (get start-time reservation-info) 
                          (get end-time reservation-info)))
    (ok refund-amount)))

(define-public (set-equipment-maintenance (equipment-id uint) (start-time uint) (end-time uint))
  (let 
    (
      (equipment-info (unwrap! (map-get? lab-equipment equipment-id) ERR-EQUIPMENT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs (get lab-id equipment-info)) ERR-INVALID-LAB))
    )
    (asserts! (is-eq tx-sender (get owner lab-info)) ERR-NOT-TOKEN-OWNER)
    (asserts! (< start-time end-time) ERR-INVALID-TIME-SLOT)
    (map-set lab-equipment equipment-id 
      (merge equipment-info 
        (tuple (maintenance-start start-time) (maintenance-end end-time))))
    (ok true)))

(define-public (toggle-equipment-status (equipment-id uint))
  (let 
    (
      (equipment-info (unwrap! (map-get? lab-equipment equipment-id) ERR-EQUIPMENT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs (get lab-id equipment-info)) ERR-INVALID-LAB))
    )
    (asserts! (is-eq tx-sender (get owner lab-info)) ERR-NOT-TOKEN-OWNER)
    (map-set lab-equipment equipment-id 
      (merge equipment-info (tuple (available (not (get available equipment-info))))))
    (ok true)))

(define-public (modify-reservation (reservation-id uint) (new-start-time uint) (new-duration-hours uint))
  (let 
    (
      (reservation-info (unwrap! (map-get? equipment-reservations reservation-id) ERR-RESERVATION-NOT-FOUND))
      (equipment-info (unwrap! (map-get? lab-equipment (get equipment-id reservation-info)) ERR-EQUIPMENT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs (get lab-id equipment-info)) ERR-INVALID-LAB))
      (new-end-time (+ new-start-time (* new-duration-hours u144)))
      (new-total-cost (* (get hourly-rate equipment-info) new-duration-hours))
      (cost-difference (if (> new-total-cost (get total-cost reservation-info))
                        (- new-total-cost (get total-cost reservation-info))
                        u0))
    )
    (asserts! (is-eq (get user reservation-info) tx-sender) ERR-NOT-TOKEN-OWNER)
    (asserts! (is-eq (get status reservation-info) u1) ERR-RESERVATION-EXPIRED)
    (asserts! (> (get start-time reservation-info) stacks-block-height) ERR-RESERVATION-EXPIRED)
    (asserts! (<= new-duration-hours (get max-reservation-hours equipment-info)) ERR-INVALID-TIME-SLOT)
    (asserts! (> new-start-time stacks-block-height) ERR-INVALID-TIME-SLOT)
    (begin
      (unblock-time-slots (get equipment-id reservation-info) 
                          (get start-time reservation-info) 
                          (get end-time reservation-info))
      (asserts! (is-time-slot-available (get equipment-id reservation-info) new-start-time new-end-time) ERR-RESERVATION-CONFLICT))
    (if (> cost-difference u0)
      (try! (stx-transfer? cost-difference tx-sender (get owner lab-info)))
      (if (< new-total-cost (get total-cost reservation-info))
        (try! (stx-transfer? (- (get total-cost reservation-info) new-total-cost) 
                            (get owner lab-info) tx-sender))
        true))
    (map-set equipment-reservations reservation-id
      (merge reservation-info
        (tuple 
          (start-time new-start-time)
          (end-time new-end-time)
          (total-cost new-total-cost))))
    (begin 
      (block-time-slots (get equipment-id reservation-info) new-start-time new-end-time reservation-id))
    (ok true)))

(define-public (create-safety-certification (name (string-ascii 64)) (description (string-ascii 256)) (validity-months uint))
  (let 
    (
      (certification-id (+ (var-get certification-count) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set safety-certifications certification-id
      (tuple
        (name name)
        (description description)
        (validity-months validity-months)
        (required-for-labs (list))
        (required-for-equipment (list))
        (created-by tx-sender)
        (active true)))
    (var-set certification-count certification-id)
    (ok certification-id)))

(define-public (authorize-instructor (instructor principal) (certification-ids (list 20 uint)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-instructors instructor certification-ids)
    (ok true)))

(define-public (issue-certification (user principal) (certification-id uint))
  (let 
    (
      (cert-info (unwrap! (map-get? safety-certifications certification-id) ERR-CERTIFICATION-NOT-FOUND))
      (instructor-certs (default-to (list) (map-get? authorized-instructors tx-sender)))
      (validity-blocks (* (get validity-months cert-info) u4320))
      (expires-at (+ stacks-block-height validity-blocks))
    )
    (asserts! (get active cert-info) ERR-INVALID-CERTIFICATION)
    (asserts! (is-some (index-of instructor-certs certification-id)) ERR-NOT-AUTHORIZED-INSTRUCTOR)
    (asserts! (is-none (map-get? user-certifications (tuple (user user) (certification-id certification-id)))) ERR-CERTIFICATION-ALREADY-EXISTS)
    (map-set user-certifications 
      (tuple (user user) (certification-id certification-id))
      (tuple
        (issued-at stacks-block-height)
        (expires-at expires-at)
        (issued-by tx-sender)
        (status u1)))
    (let 
      (
        (current-history (default-to (list) (map-get? user-certification-history user)))
      )
      (map-set user-certification-history user 
        (unwrap-panic (as-max-len? (append current-history certification-id) u100))))
    (ok true)))

(define-public (revoke-certification (user principal) (certification-id uint))
  (let 
    (
      (cert-record (unwrap! (map-get? user-certifications (tuple (user user) (certification-id certification-id))) ERR-CERTIFICATION-NOT-FOUND))
      (instructor-certs (default-to (list) (map-get? authorized-instructors tx-sender)))
    )
    (asserts! (or 
      (is-eq tx-sender (get issued-by cert-record))
      (is-eq tx-sender CONTRACT-OWNER)
      (is-some (index-of instructor-certs certification-id))) ERR-NOT-AUTHORIZED-INSTRUCTOR)
    (map-set user-certifications 
      (tuple (user user) (certification-id certification-id))
      (merge cert-record (tuple (status u3))))
    (ok true)))

(define-public (renew-certification (user principal) (certification-id uint))
  (let 
    (
      (cert-info (unwrap! (map-get? safety-certifications certification-id) ERR-CERTIFICATION-NOT-FOUND))
      (cert-record (unwrap! (map-get? user-certifications (tuple (user user) (certification-id certification-id))) ERR-CERTIFICATION-NOT-FOUND))
      (instructor-certs (default-to (list) (map-get? authorized-instructors tx-sender)))
      (validity-blocks (* (get validity-months cert-info) u4320))
      (new-expires-at (+ stacks-block-height validity-blocks))
    )
    (asserts! (get active cert-info) ERR-INVALID-CERTIFICATION)
    (asserts! (is-some (index-of instructor-certs certification-id)) ERR-NOT-AUTHORIZED-INSTRUCTOR)
    (asserts! (is-eq (get status cert-record) u1) ERR-INVALID-CERTIFICATION)
    (map-set user-certifications 
      (tuple (user user) (certification-id certification-id))
      (merge cert-record 
        (tuple 
          (issued-at stacks-block-height)
          (expires-at new-expires-at))))
    (ok true)))

(define-public (set-lab-certification-requirements (lab-id uint) (required-certs (list 10 uint)))
  (let 
    (
      (lab-info (unwrap! (map-get? labs lab-id) ERR-INVALID-LAB))
    )
    (asserts! (is-eq tx-sender (get owner lab-info)) ERR-NOT-TOKEN-OWNER)
    (map-set lab-certification-requirements lab-id required-certs)
    (ok true)))

(define-public (set-equipment-certification-requirements (equipment-id uint) (required-certs (list 10 uint)))
  (let 
    (
      (equipment-info (unwrap! (map-get? lab-equipment equipment-id) ERR-EQUIPMENT-NOT-FOUND))
      (lab-info (unwrap! (map-get? labs (get lab-id equipment-info)) ERR-INVALID-LAB))
    )
    (asserts! (is-eq tx-sender (get owner lab-info)) ERR-NOT-TOKEN-OWNER)
    (map-set equipment-certification-requirements equipment-id required-certs)
    (ok true)))

(define-public (toggle-certification-status (certification-id uint))
  (let 
    (
      (cert-info (unwrap! (map-get? safety-certifications certification-id) ERR-CERTIFICATION-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set safety-certifications certification-id 
      (merge cert-info (tuple (active (not (get active cert-info))))))
    (ok true)))

;; read only functions
(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

(define-read-only (get-token-uri (token-id uint))
  (ok none))

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? labpass token-id)))

(define-read-only (get-balance (account principal))
  (default-to u0 (map-get? token-count account)))

(define-read-only (get-pass-info (pass-id uint))
  (map-get? lab-passes pass-id))

(define-read-only (get-lab-info (lab-id uint))
  (map-get? labs lab-id))

(define-read-only (is-pass-valid (pass-id uint))
  (match (map-get? lab-passes pass-id)
    pass-info (< stacks-block-height (get expiry-block pass-info))
    false))

(define-read-only (get-user-passes (user principal))
  (default-to (list) (map-get? user-lab-history user)))

(define-read-only (get-listing-in-ustx (token-id uint))
  (match (nft-get-owner? labpass token-id)
    owner (map-get? market (tuple (token-id token-id) (owner owner)))
    none))

(define-read-only (get-lab-count)
  (var-get lab-count))

(define-read-only (get-equipment-info (equipment-id uint))
  (map-get? lab-equipment equipment-id))

(define-read-only (get-reservation-info (reservation-id uint))
  (map-get? equipment-reservations reservation-id))

(define-read-only (get-user-reservations (user principal))
  (default-to (list) (map-get? user-reservations user)))

(define-read-only (get-equipment-count)
  (var-get equipment-count))

(define-read-only (get-reservation-count)
  (var-get reservation-count))

(define-read-only (is-equipment-available (equipment-id uint) (start-time uint) (end-time uint))
  (let 
    (
      (equipment-info (default-to 
        (tuple (name "") (lab-id u0) (hourly-rate u0) (available false) 
               (maintenance-start u0) (maintenance-end u0) (max-reservation-hours u0) (requires-training false))
        (map-get? lab-equipment equipment-id)))
    )
    (and 
      (get available equipment-info)
      (not (is-in-maintenance equipment-id start-time end-time))
      (is-time-slot-available equipment-id start-time end-time))))

(define-read-only (get-equipment-by-lab (lab-id uint))
  (ok lab-id))

(define-read-only (get-certification-info (certification-id uint))
  (map-get? safety-certifications certification-id))

(define-read-only (get-user-certification (user principal) (certification-id uint))
  (map-get? user-certifications (tuple (user user) (certification-id certification-id))))

(define-read-only (get-user-certification-history (user principal))
  (default-to (list) (map-get? user-certification-history user)))

(define-read-only (get-authorized-instructor-certs (instructor principal))
  (default-to (list) (map-get? authorized-instructors instructor)))

(define-read-only (get-lab-certification-requirements (lab-id uint))
  (default-to (list) (map-get? lab-certification-requirements lab-id)))

(define-read-only (get-equipment-certification-requirements (equipment-id uint))
  (default-to (list) (map-get? equipment-certification-requirements equipment-id)))

(define-read-only (get-certification-count)
  (var-get certification-count))

(define-read-only (is-certification-valid (user principal) (certification-id uint))
  (match (map-get? user-certifications (tuple (user user) (certification-id certification-id)))
    cert-record (and 
      (is-eq (get status cert-record) u1)
      (< stacks-block-height (get expires-at cert-record)))
    false))

(define-read-only (user-has-valid-certifications (user principal) (required-certs (list 10 uint)))
  (check-all-certifications user required-certs))

(define-read-only (check-certification-status (user principal) (cert-id uint) (valid bool))
  (if valid
    (is-certification-valid user cert-id)
    false))

;; private functions
(define-private (is-owner (token-id uint) (user principal))
  (is-eq user (unwrap! (nft-get-owner? labpass token-id) false)))

(define-private (is-in-maintenance (equipment-id uint) (start-time uint) (end-time uint))
  (let 
    (
      (equipment-info (default-to 
        (tuple (name "") (lab-id u0) (hourly-rate u0) (available false) 
               (maintenance-start u0) (maintenance-end u0) (max-reservation-hours u0) (requires-training false))
        (map-get? lab-equipment equipment-id)))
      (maintenance-start (get maintenance-start equipment-info))
      (maintenance-end (get maintenance-end equipment-info))
    )
    (and 
      (> maintenance-end u0)
      (or 
        (and (>= start-time maintenance-start) (<= start-time maintenance-end))
        (and (>= end-time maintenance-start) (<= end-time maintenance-end))
        (and (< start-time maintenance-start) (> end-time maintenance-end))))))

(define-private (is-time-slot-available (equipment-id uint) (start-time uint) (end-time uint))
  (let 
    (
      (slot-1 (default-to (tuple (reserved false) (reservation-id u0)) 
                (map-get? equipment-schedule (tuple (equipment-id equipment-id) (time-slot start-time)))))
      (slot-2 (default-to (tuple (reserved false) (reservation-id u0)) 
                (map-get? equipment-schedule (tuple (equipment-id equipment-id) (time-slot (+ start-time u144))))))
      (slot-3 (default-to (tuple (reserved false) (reservation-id u0)) 
                (map-get? equipment-schedule (tuple (equipment-id equipment-id) (time-slot (+ start-time u288))))))
    )
    (and 
      (not (get reserved slot-1))
      (not (get reserved slot-2))
      (not (get reserved slot-3)))))

(define-private (block-time-slots (equipment-id uint) (start-time uint) (end-time uint) (reservation-id uint))
  (let 
    (
      (duration-blocks (/ (- end-time start-time) u144))
    )
    (map-set equipment-schedule 
      (tuple (equipment-id equipment-id) (time-slot start-time))
      (tuple (reserved true) (reservation-id reservation-id)))
    (if (> duration-blocks u1)
      (map-set equipment-schedule 
        (tuple (equipment-id equipment-id) (time-slot (+ start-time u144)))
        (tuple (reserved true) (reservation-id reservation-id)))
      true)
    (if (> duration-blocks u2)
      (map-set equipment-schedule 
        (tuple (equipment-id equipment-id) (time-slot (+ start-time u288)))
        (tuple (reserved true) (reservation-id reservation-id)))
      true)
    true))

(define-private (unblock-time-slots (equipment-id uint) (start-time uint) (end-time uint))
  (let 
    (
      (duration-blocks (/ (- end-time start-time) u144))
    )
    (map-delete equipment-schedule (tuple (equipment-id equipment-id) (time-slot start-time)))
    (if (> duration-blocks u1)
      (map-delete equipment-schedule (tuple (equipment-id equipment-id) (time-slot (+ start-time u144))))
      true)
    (if (> duration-blocks u2)
      (map-delete equipment-schedule (tuple (equipment-id equipment-id) (time-slot (+ start-time u288))))
      true)
    true))

(define-private (has-required-certifications (user principal) (lab-id uint))
  (let 
    (
      (required-certs (default-to (list) (map-get? lab-certification-requirements lab-id)))
    )
    (if (is-eq (len required-certs) u0)
      true
      (check-all-certifications user required-certs))))

(define-private (has-equipment-certifications (user principal) (equipment-id uint))
  (let 
    (
      (required-certs (default-to (list) (map-get? equipment-certification-requirements equipment-id)))
    )
    (if (is-eq (len required-certs) u0)
      true
      (check-all-certifications user required-certs))))

(define-private (check-all-certifications (user principal) (cert-list (list 10 uint)))
  (let 
    (
      (first-cert (unwrap! (element-at cert-list u0) true))
      (second-cert (element-at cert-list u1))
      (third-cert (element-at cert-list u2))
    )
    (and 
      (is-certification-valid user first-cert)
      (match second-cert cert-id (is-certification-valid user cert-id) true)
      (match third-cert cert-id (is-certification-valid user cert-id) true))))

(define-trait commission-trait
  (
    (pay (uint uint) (response bool uint))
  ))



  