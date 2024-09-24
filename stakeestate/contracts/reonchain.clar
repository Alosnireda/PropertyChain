(define-data-var property-owner principal tx-sender)
(define-data-var potential-buyer (optional principal) none)
(define-data-var property-cost uint 0)
(define-data-var transaction-complete bool false)
(define-data-var property-asset-id uint 0)
(define-data-var listing-expiration uint 0)
(define-data-var multi-signature-approvers (list 3 principal) (list))
(define-data-var approvers-count uint 0)

;; Define an NFT to represent the property asset
(define-non-fungible-token real-estate-nft uint)

;; Define a trait for transferring the property NFT
(define-trait asset-transfer-trait
  ((transfer (uint principal principal) (response bool uint))))

;; 1. Function to get the current block height
(define-read-only (get-current-block-height)
  (ok (get-block-info? height)))

;; 2. Function to check if a principal is in the list of approvers
(define-read-only (is-in-list (approver principal) (approvers (list 3 principal)))
  (ok (not (is-eq (len (filter (lambda (x) (is-eq x approver)) approvers)) u0))))

;; 3. Function to list a property with a given cost, asset ID, and listing duration
(define-public (register-property (cost uint) (asset-id uint) (duration uint))
  (let ((current-block-height (unwrap! (get-current-block-height) (err u120))))
    (begin
      (asserts! (is-eq (var-get property-owner) tx-sender) (err u100))
      (asserts! (is-eq (var-get transaction-complete) false) (err u101))
      (var-set property-cost cost)
      (var-set property-asset-id asset-id)
      ;; Set the listing expiration date (current block height + duration)
      (var-set listing-expiration (+ current-block-height duration))
      (print (list "Property registered successfully" 
                   (var-get property-owner) 
                   (var-get property-cost) 
                   (var-get property-asset-id) 
                   (var-get listing-expiration)))
      (ok "Property registered successfully")
    )
  )
)

;; 4. Function to express interest in buying the property
(define-public (submit-offer)
  (let ((current-block-height (unwrap! (get-current-block-height) (err u120))))
    (begin
      (asserts! (is-none (var-get potential-buyer)) (err u102))
      (asserts! (is-eq (var-get transaction-complete) false) (err u103))
      (asserts! (> (var-get listing-expiration) current-block-height) (err u110)) ;; Check listing is still active
      (var-set potential-buyer (some tx-sender))
      (ok "Offer submitted successfully")
    )
  )
)

;; 5. Function to add a multi-signature approver
(define-public (add-approver (approver principal))
  (begin
    (asserts! (is-eq (var-get property-owner) tx-sender) (err u111))
    (asserts! (< (len (var-get multi-signature-approvers)) u3) (err u112))
    (var-set multi-signature-approvers (append (var-get multi-signature-approvers) (list approver)))
    (var-set approvers-count (+ (var-get approvers-count) u1))
    (ok "Approver added successfully")
  )
)

;; 6. Function to approve the transaction
(define-public (approve-transaction)
  (let ((is-approved (unwrap! (is-in-list tx-sender (var-get multi-signature-approvers)) (err u113))))
    (begin
      (asserts! (is-eq (var-get transaction-complete) false) (err u106))
      (asserts! is-approved (err u113)) ;; Check if the sender is in the list of approvers
      (var-set approvers-count (+ (var-get approvers-count) u1))
      (ok "Transaction approved")
    )
  )
)

;; 7. Function to complete the sale and transfer the property with multi-signature approval
(define-public (complete-transaction)
  (let ((buyer (unwrap! (var-get potential-buyer) (err u104)))
        (cost (var-get property-cost))
        (asset-id (var-get property-asset-id))
        (is-complete (var-get transaction-complete)))
    (begin
      (asserts! (is-eq tx-sender buyer) (err u105))
      (asserts! (is-eq is-complete false) (err u106))
      (asserts! (>= (var-get approvers-count) u2) (err u114)) ;; At least two approvals required
      ;; Transfer the property NFT from owner to buyer
      (try! (nft-transfer? real-estate-nft asset-id (var-get property-owner) buyer))
      ;; Transfer funds from buyer to owner
      (try! (stx-transfer? cost buyer (var-get property-owner)))
      ;; Mark transaction as complete
      (var-set transaction-complete true)
      (print (list "Transaction completed" 
                   (var-get property-owner) 
                   buyer 
                   cost 
                   asset-id))
      (ok "Transaction completed successfully")
    )
  )
)

;; 8. Function to withdraw the offer and reset the potential buyer
(define-public (withdraw-offer)
  (let ((buyer (unwrap! (var-get potential-buyer) (err u107))))
    (asserts! (is-eq tx-sender buyer) (err u108))
    (asserts! (is-eq (var-get transaction-complete) false) (err u109))
    (var-set potential-buyer none)
    (ok "Offer withdrawn successfully")
  )
)

;; 9. Function to update the property cost
(define-public (update-property-cost (new-cost uint))
  (begin
    (asserts! (is-eq (var-get property-owner) tx-sender) (err u115))
    (asserts! (is-eq (var-get transaction-complete) false) (err u116))
    (var-set property-cost new-cost)
    (ok "Property cost updated successfully")
  )
)

;; 10. Function to cancel the listing if expired
(define-public (cancel-listing-if-expired)
  (let ((current-block-height (unwrap! (get-current-block-height) (err u120))))
    (begin
      (asserts! (is-eq (var-get transaction-complete) false) (err u117))
      (asserts! (>= current-block-height (var-get listing-expiration)) (err u118))
      (var-set potential-buyer none)
      (var-set property-cost 0)
      (var-set property-asset-id 0)
      (var-set listing-expiration 0)
      (ok "Listing cancelled due to expiration")
    )
  )
)

;; 11. Function to resolve disputes by resetting the transaction
(define-public (reset-transaction)
  (begin
    (asserts! (is-eq (var-get property-owner) tx-sender) (err u119))
    (var-set transaction-complete false)
    (var-set potential-buyer none)
    (var-set approvers-count 0)
    (ok "Transaction reset successfully")
  )
)
