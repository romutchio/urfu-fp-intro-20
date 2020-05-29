module API.Checkout where

import Servant.API
import DB.Booking

type CheckoutAPI
  = "api" :> "checkout"
    :> Capture "id" BookingId
    :> Get '[JSON] String
  :<|>
    ("api" :> "refund" 
    :> Capture "id" BookingId 
    :> Get '[JSON] String)
