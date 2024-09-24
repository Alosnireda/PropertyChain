(define-data-var property-owner principal tx-sender)
(define-data-var potential-buyer (optional principal) none)
(define-data-var property-cost uint u0)
(define-data-var transaction-complete bool false)
(define-data-var property-asset-id uint u0)
(define-data-var listing-expiration uint u0)
(define-data-var approvers-count uint u0)

;; Define an NFT to represent the property asset
(define-non-fungible-token real-estate-nft uint)

;; Define a trait for transferring the property NFT
(define-trait asset-transfer-trait
  ((transfer (uint principal principal) (response bool uint))))

;; Function to get the current block time
(define-read-only (get-current-block-time)
  (ok (get-block-info? time u0)))

;; Helper function to check if a principal is equal to another
(define-read-only (is-eq-principal (p1 principal) (p2 principal))
  (is-eq p1 p2))

;; Function to validate a uint value
(define-read-only (validate-uint (value uint))
  (begin
    (asserts! (>= value u0) (err u200))
    (ok value)
  )
)

;; Function to validate a property cost
(define-read-only (validate-property-cost (cost uint))
  (begin
    ;; Ensure cost is not zero or too high (example limit: 10_000_000 STX)
    (asserts! (> cost u0) (err u201))
    (asserts! (<= cost u10000000) (err u202))
    (ok cost)
  )
)

;; Function to validate a listing duration
(define-read-only (validate-duration (duration uint))
  (begin
    ;; Ensure duration is within an acceptable range (example: 1 to 100,000 blocks)
    (asserts! (>= duration u1) (err u203))
    (asserts! (<= duration u100000) (err u204))
    (ok duration)
  )
)

;; Function to list a property with a given cost, asset ID, and listing duration
(define-public (register-property (cost uint) (asset-id uint) (duration uint))
  (let ((current-block-time (unwrap! (get-current-block-time) (err u120))))
    (let ((block-time (unwrap! current-block-time (err u121)))) ;; Unwrap the optional value
      (begin
        ;; Validate inputs and store validated values in local variables
        (unwrap-panic (validate-property-cost cost))
        (unwrap-panic (validate-uint asset-id))
        (unwrap-panic (validate-duration duration))

        ;; Verify ownership and transaction status
        (asserts! (is-eq (var-get property-owner) tx-sender) (err u100))
        (asserts! (is-eq (var-get transaction-complete) false) (err u101))

        ;; Inline asserts before setting variables
        (asserts! (> cost u0) (err u201))
        (asserts! (<= cost u10000000) (err u202))
        (var-set property-cost cost)
        
        (asserts! (>= asset-id u0) (err u200))
        (var-set property-asset-id asset-id)

        (asserts! (>= duration u1) (err u203))
        (asserts! (<= duration u100000) (err u204))
        (var-set listing-expiration (+ block-time duration))

        (print "Property registered successfully.")
        (ok "Property registered successfully")
      )
    )
  )
)

;; Function to express interest in buying the property
(define-public (submit-offer)
  (let ((current-block-time (unwrap! (get-current-block-time) (err u120))))
    (begin
      (let ((block-time (unwrap! current-block-time (err u121))))
        (asserts! (is-none (var-get potential-buyer)) (err u102))
        (asserts! (is-eq (var-get transaction-complete) false) (err u103))
        (asserts! (> (var-get listing-expiration) block-time) (err u110)) ;; Use unwrapped value here
        (var-set potential-buyer (some tx-sender))
        (ok "Offer submitted successfully")
      )
    )
  )
)

;; Function to approve the transaction (single approval for simplicity)
(define-public (approve-transaction)
  (begin
    (asserts! (is-eq (var-get transaction-complete) false) (err u106))
    ;; Increment the approval count
    (var-set approvers-count (+ (var-get approvers-count) u1))
    (ok "Transaction approved")
  )
)

;; Function to complete the sale and transfer the property with approval
(define-public (complete-transaction)
  (let ((buyer (unwrap! (var-get potential-buyer) (err u104)))
        (cost (var-get property-cost))
        (asset-id (var-get property-asset-id))
        (is-complete (var-get transaction-complete)))
    (begin
      (asserts! (is-eq tx-sender buyer) (err u105))
      (asserts! (is-eq is-complete false) (err u106))
      ;; Transfer the property NFT from owner to buyer
      (try! (nft-transfer? real-estate-nft asset-id (var-get property-owner) buyer))
      ;; Transfer funds from buyer to owner
      (try! (stx-transfer? cost buyer (var-get property-owner)))
      ;; Mark transaction as complete
      (var-set transaction-complete true)
      (print "Transaction completed successfully")
      (ok "Transaction completed successfully")
    )
  )
)

;; Function to withdraw the offer and reset the potential buyer
(define-public (withdraw-offer)
  (let ((buyer (unwrap! (var-get potential-buyer) (err u107))))
    (asserts! (is-eq tx-sender buyer) (err u108))
    (asserts! (is-eq (var-get transaction-complete) false) (err u109))
    (var-set potential-buyer none)
    (ok "Offer withdrawn successfully")
  )
)

;; Function to update the property cost
(define-public (update-property-cost (new-cost uint))
  (begin
    ;; Validate the new cost before updating
    (unwrap-panic (validate-property-cost new-cost))
    ;; Inline asserts before setting variables
    (asserts! (> new-cost u0) (err u201))
    (asserts! (<= new-cost u10000000) (err u202))
    
    (asserts! (is-eq (var-get property-owner) tx-sender) (err u115))
    (asserts! (is-eq (var-get transaction-complete) false) (err u116))
    ;; Set validated values
    (var-set property-cost new-cost)
    (ok "Property cost updated successfully")
  )
)

;; Function to cancel the listing if expired
(define-public (cancel-listing-if-expired)
  (let ((current-block-time (unwrap! (get-current-block-time) (err u120))))
    (let ((block-time (unwrap! current-block-time (err u121))))
      (begin
        (asserts! (is-eq (var-get transaction-complete) false) (err u117))
        (asserts! (>= block-time (var-get listing-expiration)) (err u118)) ;; Use unwrapped value here
        (var-set potential-buyer none)
        (var-set property-cost u0)
        (var-set property-asset-id u0)
        (var-set listing-expiration u0)
        (ok "Listing cancelled due to expiration")
      )
    )
  )
)

;; Function to resolve disputes by resetting the transaction
(define-public (reset-transaction)
  (begin
    (asserts! (is-eq (var-get property-owner) tx-sender) (err u119))
    (var-set transaction-complete false)
    (var-set potential-buyer none)
    (var-set approvers-count u0)
    (ok "Transaction reset successfully")
  )
)
