
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

;; data vars
(define-data-var last-token-id uint u0)
(define-data-var lab-count uint u0)
(define-data-var commission uint u250)

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

;; private functions
(define-private (is-owner (token-id uint) (user principal))
  (is-eq user (unwrap! (nft-get-owner? labpass token-id) false)))

(define-trait commission-trait
  (
    (pay (uint uint) (response bool uint))
  ))