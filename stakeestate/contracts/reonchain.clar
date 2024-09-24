(define-data-var property-owner principal tx-sender)
(define-data-var potential-buyer (optional principal) none)
(define-data-var property-cost uint 0)
(define-data-var transaction-complete bool false)
(define-data-var property-asset-id uint 0)

;; Define an NFT to represent the property asset
(define-non-fungible-token real-estate-nft uint)

;; Define a trait for transferring the property NFT
(define-trait asset-transfer-trait
  ((transfer (uint principal principal) (response bool uint))))

;; Function to list a property with a given cost and asset ID
(define-public (register-property (cost uint) (asset-id uint))
  (begin
    (asserts! (is-eq (var-get property-owner) tx-sender) (err u100))
    (asserts! (is-eq (var-get transaction-complete) false) (err u101))
    (var-set property-cost cost)
    (var-set property-asset-id asset-id)
    (ok "Property registered successfully")
  )
)

;; Function to express interest in buying the property
(define-public (submit-offer)
  (begin
    (asserts! (is-none (var-get potential-buyer)) (err u102))
    (asserts! (is-eq (var-get transaction-complete) false) (err u103))
    (var-set potential-buyer (some tx-sender))
    (ok "Offer submitted successfully")
  )
)

;; Function to finalize the sale and transfer the property
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
