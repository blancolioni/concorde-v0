with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Concorde.Agents;
with Concorde.Stock;
with Concorde.Worlds;

with Concorde.Logging;

with Concorde.Handles.Historical_Offer;
with Concorde.Handles.Lease_Contract;
with Concorde.Handles.Market;
with Concorde.Handles.Market_Offer;
with Concorde.Handles.Transaction;

package body Concorde.Markets is

   procedure Execute_Transaction
     (Market                        : Concorde.Handles.Market_Reference;
      Buyer, Seller                 : Concorde.Handles.Agent_Reference;
      Buyer_Account, Seller_Account : Concorde.Handles.Account_Reference;
      Buyer_Stock, Seller_Stock     : Concorde.Handles.Has_Stock_Reference;
      Commodity                     : Concorde.Commodities.Commodity_Reference;
      Quantity                      : Concorde.Quantities.Quantity_Type;
      Price                         : Concorde.Money.Price_Type);

   procedure Execute_Offers
     (Market    : Concorde.Handles.Market_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference);

   type Weighted_Offer is
      record
         Weight   : Non_Negative_Real;
         Quantity : Concorde.Quantities.Quantity_Type;
      end record;

   package Weighted_Offer_Vectors is
     new Ada.Containers.Vectors (Positive, Weighted_Offer);

   procedure Allocate_Quantity
     (Quantity : Concorde.Quantities.Quantity_Type;
      Offers   : in out Weighted_Offer_Vectors.Vector);

   function Offer_Priority
     (Offer : Concorde.Handles.Offer_Type;
      Price : Concorde.Money.Price_Type)
      return Real
   is (case Offer is
          when Concorde.Handles.Ask =>
             Concorde.Money.To_Real (Price),
          when Concorde.Handles.Bid =>
             1.0 / Concorde.Money.To_Real (Price));

   function Name
     (Offer : Concorde.Handles.Offer_Type)
      return String
   is (case Offer is
          when Concorde.Handles.Ask => "bid",
          when Concorde.Handles.Bid => "ask")
        with Unreferenced;

   function "not"
     (Offer : Concorde.Handles.Offer_Type)
      return Concorde.Handles.Offer_Type
   is (case Offer is
          when Concorde.Handles.Ask => Concorde.Handles.Bid,
          when Concorde.Handles.Bid => Concorde.Handles.Ask)
        with Unreferenced;

   -----------------------
   -- Allocate_Quantity --
   -----------------------

   procedure Allocate_Quantity
     (Quantity : Concorde.Quantities.Quantity_Type;
      Offers   : in out Weighted_Offer_Vectors.Vector)
   is
      use Concorde.Quantities;
      Qs : array (1 .. Offers.Last_Index) of Quantity_Type;
      Ws : array (1 .. Offers.Last_Index) of Non_Negative_Real;
      T  : Quantity_Type := Zero;
      R  : Quantity_Type := Quantity;
      W  : Non_Negative_Real := 0.0;
   begin

      for I in 1 .. Offers.Last_Index loop
         Qs (I) := Offers (I).Quantity;
         Ws (I) := Offers (I).Weight;
         T := T + Qs (I);
         W := W + Ws (I);
      end loop;

      R := Quantity;

      if T <= R then
         R := Zero;
         for Item of Qs loop
            Item := Zero;
         end loop;
      else
         declare
            F : constant Unit_Real :=
              To_Real (R) / To_Real (T);
         begin
            for I in Qs'Range loop
               declare
                  This_Q : constant Quantity_Type :=
                    Min (Min (Scale (T, F * Ws (I) / W), Qs (I)), R);
               begin
                  R := R - This_Q;
                  Qs (I) := Qs (I) - This_Q;
               end;
            end loop;
         end;

         if R > Zero then
            for I in Qs'Range loop
               declare
                  This_Q : constant Quantity_Type :=
                    Min (R, Qs (I));
               begin
                  R := R - This_Q;
                  Qs (I) := Qs (I) - This_Q;
                  exit when R = Zero;
               end;
            end loop;
         end if;
      end if;

      for I in 1 .. Offers.Last_Index loop
         Offers (I).Quantity := Offers (I).Quantity - Qs (I);
      end loop;

   end Allocate_Quantity;

   ------------------
   -- Create_Offer --
   ------------------

   procedure Create_Offer
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Has_Stock : Concorde.Handles.Has_Stock_Reference;
      Account   : Concorde.Handles.Account_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
      Ref : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin

      Concorde.Handles.Historical_Offer.Create
        (Time_Stamp  => Concorde.Calendar.Clock,
         Time_Offset =>
           10_000.0
         / (1.0 + Concorde.Calendar.To_Real (Concorde.Calendar.Clock)),
         Market      => Market,
         Commodity   => Ref,
         Agent       => Agent,
         Offer       => Offer,
         Quantity    => Quantity,
         Price       => Price);

      declare
         use type Concorde.Handles.Market_Offer_Reference;
         Old_Offer : constant Concorde.Handles.Market_Offer_Reference :=
           Concorde.Handles.Market_Offer.Get_By_Market_Offer
             (Market, Agent, Ref);
         Priority  : constant Real :=
           Offer_Priority (Offer, Price);
      begin
         if Old_Offer /= Concorde.Handles.Null_Market_Offer_Reference then
            Concorde.Handles.Market_Offer.Update_Market_Offer (Old_Offer)
              .Set_Offer (Offer)
              .Set_Original (Quantity)
              .Set_Quantity (Quantity)
              .Set_Priority (Priority)
              .Set_Price (Price)
              .Done;
         else
            Concorde.Handles.Market_Offer.Create
              (Market    => Market,
               Priority  => Priority,
               Commodity => Ref,
               Offer     => Offer,
               Agent     => Agent,
               Has_Stock => Has_Stock,
               Account   => Account,
               Original  => Quantity,
               Quantity  => Quantity,
               Price     => Price);
         end if;
      end;

      Execute_Offers (Market, Commodity);

   end Create_Offer;

   -------------------
   -- Current_Price --
   -------------------

   function Current_Price
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Money, Concorde.Quantities;
      Ref        : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
      Remaining  : Quantity_Type := Quantity;
      Last_Price : Price_Type := Zero;
   begin
      for Market_Offer of
        Concorde.Handles.Market_Offer.Select_Priority_Offer_Bounded_By_Priority
          (Market          => Market,
           Commodity       => Ref,
           Offer           => Offer,
           Start_Priority  => 0.0,
           Finish_Priority => Real'Last)
      loop
         Last_Price := Market_Offer.Price;
         if Market_Offer.Quantity >= Remaining then
            return Market_Offer.Price;
         else
            Remaining := Remaining - Market_Offer.Quantity;
         end if;
      end loop;

      if Last_Price = Zero then
         return Concorde.Commodities.Initial_Price (Commodity);
      else
         return Last_Price;
      end if;

   end Current_Price;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      Ref : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin
      return Quantity : Quantity_Type := Zero do
         for Item of
           Concorde.Handles.Market_Offer.Select_Priority_Offer_Bounded_By_Priority
             (Market, Ref, Offer, 0.0, Real'Last)
         loop
            Quantity := Quantity + Item.Quantity;
         end loop;
      end return;
   end Current_Quantity;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Cash      : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Money, Concorde.Quantities;
      Remaining : Money_Type := Cash;
      Ref       : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin
      return Quantity : Quantity_Type := Zero do
         for Item of
           Concorde.Handles.Market_Offer.Select_Priority_Offer_Bounded_By_Priority
             (Market, Ref, Offer, 0.0, Real'Last)
         loop
            declare
               This_Cost : constant Money_Type :=
                 Total (Item.Price, Item.Quantity);
            begin
               if This_Cost >= Remaining then
                  Quantity := Quantity
                    + Get_Quantity (Remaining, Item.Price);
                  exit;
               else
                  Remaining := Remaining - This_Cost;
                  Quantity := Quantity + Item.Quantity;
               end if;
            end;
         end loop;
      end return;
   end Current_Quantity;

   ----------------------
   -- Current_Buy_Cost --
   ----------------------

   function Current_Value
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type
   is
      use Concorde.Money, Concorde.Quantities;
      Remaining : Quantity_Type := Quantity;
      Ref       : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin
      return Cost : Money_Type := Zero do
         for Item of
           Concorde.Handles.Market_Offer.Select_Priority_Offer_Bounded_By_Priority
             (Market, Ref, Offer, 0.0, Real'Last)
         loop
            declare
               This_Quantity : constant Quantity_Type := Item.Quantity;
            begin
               if This_Quantity >= Remaining then
                  Cost := Cost
                    + Total (Item.Price, Remaining);
                  exit;
               else
                  Remaining := Remaining - This_Quantity;
                  Cost := Cost + Total (Item.Price, This_Quantity);
               end if;
            end;
         end loop;
      end return;
   end Current_Value;

   --------------------
   -- Execute_Offers --
   --------------------

   procedure Execute_Offers
     (Market    : Concorde.Handles.Market_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference)
   is
      use Concorde.Money, Concorde.Quantities;

      Ref : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);

      type Offer_Type is
         record
            Agent     : Concorde.Handles.Agent_Reference;
            Account   : Concorde.Handles.Account_Reference;
            Has_Stock : Concorde.Handles.Has_Stock_Reference;
            Offer     : Concorde.Handles.Market_Offer_Reference;
            Quantity  : Quantity_Type;
         end record;

      package Offer_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Offer_Type);

      type Offer_At_Price is
         record
            Price : Price_Type;
            Total : Quantity_Type;
            List  : Offer_Lists.List;
         end record;

      package Offer_Maps is
        new Ada.Containers.Ordered_Maps
          (Price_Type, Offer_At_Price, "<");

      procedure Add_To_Queue
        (Queue     : in out Offer_Maps.Map;
         Agent     : Concorde.Handles.Agent_Reference;
         Account   : Concorde.Handles.Account_Reference;
         Has_Stock : Concorde.Handles.Has_Stock_Reference;
         Offer     : Concorde.Handles.Market_Offer_Reference;
         Price     : Price_Type;
         Quantity  : Quantity_Type);

      procedure Execute
        (Ask, Bid : in out Offer_At_Price);

      ------------------
      -- Add_To_Queue --
      ------------------

      procedure Add_To_Queue
        (Queue     : in out Offer_Maps.Map;
         Agent     : Concorde.Handles.Agent_Reference;
         Account   : Concorde.Handles.Account_Reference;
         Has_Stock : Concorde.Handles.Has_Stock_Reference;
         Offer     : Concorde.Handles.Market_Offer_Reference;
         Price     : Price_Type;
         Quantity  : Quantity_Type)
      is
         use Offer_Maps;
         Position : constant Cursor := Queue.Find (Price);
         Rec      : constant Offer_Type :=
           (Agent, Account, Has_Stock, Offer, Quantity);
      begin
--           Log_Market (Market, "offer" & Db.To_String (Offer)
--                       & " added to queue");
         if not Has_Element (Position) then
            declare
               New_Item : Offer_At_Price := (Price, Quantity, List => <>);
            begin
               New_Item.List.Append (Rec);
               Queue.Insert (Price, New_Item);
            end;
         else
            declare
               Item : Offer_At_Price renames Queue (Position);
            begin
               Item.Total := Item.Total + Quantity;
               Item.List.Append (Rec);
            end;
         end if;
      end Add_To_Queue;

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Ask, Bid : in out Offer_At_Price)
      is
         Total       : constant Quantity_Type := Min (Ask.Total, Bid.Total);
         Price       : constant Price_Type :=
           Adjust_Price (Ask.Price + Bid.Price, 0.5);

         package Offer_List_Vectors is
           new Ada.Containers.Vectors (Positive, Offer_Lists.Cursor,
                                       Offer_Lists."=");

         type Offer_Record is
            record
               Weighted_Vector : Weighted_Offer_Vectors.Vector;
               Positions       : Offer_List_Vectors.Vector;
            end record;

         Asks, Bids : Offer_Record;

      begin
         for Position in Ask.List.Iterate loop
            Asks.Weighted_Vector.Append ((1.0, Ask.List (Position).Quantity));
            Asks.Positions.Append (Position);
         end loop;

         Allocate_Quantity (Total, Asks.Weighted_Vector);

         for Position in Bid.List.Iterate loop
            Bids.Weighted_Vector.Append ((1.0, Bid.List (Position).Quantity));
            Bids.Positions.Append (Position);
         end loop;

         Allocate_Quantity (Total, Bids.Weighted_Vector);

         for I in 1 .. Asks.Weighted_Vector.Last_Index loop
            declare
               Offer : constant Concorde.Handles.Market_Offer.Market_Offer_Type :=
                 Concorde.Handles.Market_Offer.Get
                   (Ask.List (Asks.Positions.Element (I)).Offer);
               Ask_Quantity : constant Quantity_Type :=
                 Asks.Weighted_Vector.Element (I).Quantity;
               Previous     : constant Quantity_Type := Offer.Quantity;
               Remaining    : constant Quantity_Type :=
                 Previous - Ask_Quantity;
            begin
--                 Log_Market
--                   (Market,
--                    "ask: agent" & Concorde.Handles.To_String (Offer.Agent)
--                    & " offer"
--                    & Concorde.Handles.To_String
--                      (Offer.Get_Market_Offer_Reference)
--                    & " "
--                    & Concorde.Handles.Record_Type'Image
--                      (Offer.Top_Record)
--                    & " quantity " & Show (Ask_Quantity)
--                    & " previous " & Show (Previous)
--                    & " remaining " & Show (Remaining));
               pragma Assert (Ask_Quantity <= Previous);
               Concorde.Handles.Market_Offer.Update_Market_Offer
                 (Offer.Get_Market_Offer_Reference)
                 .Set_Quantity (Remaining)
                 .Done;
            end;
         end loop;

         for I in 1 .. Bids.Weighted_Vector.Last_Index loop
            declare
               Offer : constant Concorde.Handles.Market_Offer.Market_Offer_Type :=
                 Concorde.Handles.Market_Offer.Get
                   (Bid.List (Bids.Positions.Element (I)).Offer);
               Bid_Quantity : constant Quantity_Type :=
                 Bids.Weighted_Vector.Element (I).Quantity;
               Previous     : constant Quantity_Type := Offer.Quantity;
               Remaining    : constant Quantity_Type :=
                 Previous - Bid_Quantity;
            begin
--                 Log_Market
--                   (Market,
--                    "bid: agent" & Concorde.Handles.To_String (Offer.Agent)
--                    & " offer"
--                    & Concorde.Handles.To_String
--                      (Offer.Get_Market_Offer_Reference)
--                    & " "
--                    & Concorde.Handles.Record_Type'Image
--                      (Offer.Top_Record)
--                    & " quantity " & Show (Bid_Quantity)
--                    & " previous " & Show (Previous)
--                    & " remaining " & Show (Remaining));
               pragma Assert (Bid_Quantity <= Previous);
               Concorde.Handles.Market_Offer.Update_Market_Offer
                 (Offer.Get_Market_Offer_Reference)
                 .Set_Quantity (Remaining)
                 .Done;
            end;
         end loop;

         declare
            Ask_Index : Positive := 1;
            Bid_Index : Positive := 1;
            Ask_Quantity : Quantity_Type :=
              Asks.Weighted_Vector.Element (Ask_Index).Quantity;
            Bid_Quantity : Quantity_Type :=
              Bids.Weighted_Vector.Element (Bid_Index).Quantity;
         begin

            loop
               declare
                  Quantity : constant Quantity_Type :=
                    Min (Ask_Quantity, Bid_Quantity);
                  Current_Ask : Offer_Type renames
                    Ask.List (Asks.Positions.Element (Ask_Index));
                  Current_Bid : Offer_Type renames
                    Bid.List (Bids.Positions.Element (Bid_Index));
               begin

                  Execute_Transaction
                    (Market         => Market,
                     Buyer          => Current_Bid.Agent,
                     Seller         => Current_Ask.Agent,
                     Buyer_Account  => Current_Bid.Account,
                     Seller_Account => Current_Ask.Account,
                     Buyer_Stock    => Current_Bid.Has_Stock,
                     Seller_Stock   => Current_Ask.Has_Stock,
                     Commodity      => Commodity,
                     Quantity       => Quantity,
                     Price          => Price);

                  Ask_Quantity := Ask_Quantity - Quantity;
                  Bid_Quantity := Bid_Quantity - Quantity;

                  if Ask_Quantity = Zero then
                     Ask_Index := Ask_Index + 1;
                     if Ask_Index <= Asks.Weighted_Vector.Last_Index then
                        Ask_Quantity :=
                          Asks.Weighted_Vector.Element (Ask_Index).Quantity;
                     end if;
                  end if;

                  if Bid_Quantity = Zero then
                     Bid_Index := Bid_Index + 1;
                     if Bid_Index <= Bids.Weighted_Vector.Last_Index then
                        Bid_Quantity :=
                          Bids.Weighted_Vector.Element (Bid_Index).Quantity;
                     end if;
                  end if;

                  exit when Ask_Index > Asks.Weighted_Vector.Last_Index
                    or else Bid_Index > Bids.Weighted_Vector.Last_Index;
               end;

            end loop;

            if Ask_Quantity > Zero then
               Log_Market
                 (Market,
                  Show (Ask_Quantity)
                  & " " & Concorde.Commodities.Local_Name (Commodity)
                  & " asks left over");
            end if;

            if Bid_Quantity > Zero then
               Log_Market
                 (Market,
                  Show (Bid_Quantity)
                  & " " & Concorde.Commodities.Local_Name (Commodity)
                  & " bids left over");
            end if;
         end;

         Ask.Total := Ask.Total - Total;
         Bid.Total := Bid.Total - Total;

      end Execute;

      Queues : array (Concorde.Handles.Offer_Type) of Offer_Maps.Map;

      Ask_Queue : Offer_Maps.Map renames Queues (Concorde.Handles.Ask);
      Bid_Queue : Offer_Maps.Map renames Queues (Concorde.Handles.Bid);

   begin

      for Active of
        Concorde.Handles.Market_Offer.Select_By_Market_Commodity
          (Market, Ref)
      loop
         if Active.Quantity > Zero then
--              Log_Market
--                (Market,
--                 Name (Active.Offer)
--                 & ": offer"
--                 & Db.To_String (Active.Get_Market_Offer_Reference));

            Add_To_Queue
              (Queues (Active.Offer),
               Active.Agent, Active.Account,
               Active.Has_Stock,
               Active.Get_Market_Offer_Reference,
               Active.Price, Active.Quantity);
         end if;
      end loop;

      while not Ask_Queue.Is_Empty
        and then not Bid_Queue.Is_Empty
        and then Ask_Queue.First_Element.Price
          <= Bid_Queue.Last_Element.Price
      loop
         declare
            Ask : Offer_At_Price renames Ask_Queue (Ask_Queue.First);
            Bid : Offer_At_Price renames Bid_Queue (Bid_Queue.Last);
         begin
            Execute (Ask, Bid);
         end;

         while not Ask_Queue.Is_Empty
           and then Ask_Queue.First_Element.Total = Zero
         loop
            Ask_Queue.Delete_First;
         end loop;

         while not Bid_Queue.Is_Empty
           and then Bid_Queue.Last_Element.Total = Zero
         loop
            Bid_Queue.Delete_Last;
         end loop;
      end loop;

   end Execute_Offers;

   -------------------------
   -- Execute_Transaction --
   -------------------------

   procedure Execute_Transaction
     (Market                        : Concorde.Handles.Market_Reference;
      Buyer, Seller                 : Concorde.Handles.Agent_Reference;
      Buyer_Account, Seller_Account : Concorde.Handles.Account_Reference;
      Buyer_Stock, Seller_Stock     : Concorde.Handles.Has_Stock_Reference;
      Commodity                     : Concorde.Commodities.Commodity_Reference;
      Quantity                      : Concorde.Quantities.Quantity_Type;
      Price                         : Concorde.Money.Price_Type)
   is
      Total : constant Concorde.Money.Money_Type :=
        Concorde.Money.Total (Price, Quantity);
   begin
      Log_Market
        (Market,
         "Agent" & Concorde.Handles.To_String (Buyer)
         & " buys from "
         & "Agent" & Concorde.Handles.To_String (Seller)
         & " "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Concorde.Commodities.Local_Name (Commodity)
         & " for "
         & Concorde.Money.Show (Price)
         & " ea; total "
         & Concorde.Money.Show (Total));

      Concorde.Stock.Remove_Stock
        (Seller_Stock, Commodity, Quantity);
      Concorde.Stock.Add_Stock
        (Buyer_Stock, Commodity, Quantity, Total);
      Concorde.Agents.Add_Cash
        (Seller_Account, Total, "sell-stock");
      Concorde.Agents.Remove_Cash
        (Buyer_Account, Total, "buy-stock");

      if Concorde.Commodities.Is_Lease (Commodity) then
         declare
            use Concorde.Commodities;
            use type Concorde.Calendar.Time;
            Leased_Item  : constant Commodity_Reference :=
              Leased_Commodity (Commodity);
            Leased_Value : Concorde.Money.Money_Type;

         begin

            Log_Market (Market,
                        "transferring leased commodity "
                        & Local_Name (Leased_Item));

            Concorde.Stock.Remove_Stock
              (Seller_Stock, Leased_Item, Quantity, Leased_Value);
            Concorde.Stock.Add_Stock
              (Buyer_Stock, Leased_Item, Quantity, Leased_Value);

            Concorde.Handles.Lease_Contract.Create
              (Commodity  =>
                 Concorde.Commodities.To_Database_Reference (Leased_Item),
               Owner      => Seller,
               Tenant     => Buyer,
               Expires    =>
                 Concorde.Calendar.Clock
               + Concorde.Calendar.Days (Lease_Days (Commodity)),
               Daily_Rent => Total);
         end;
      end if;

      Concorde.Handles.Transaction.Create
        (Time_Stamp => Concorde.Calendar.Clock,
         Market     => Market,
         Commodity  => Concorde.Commodities.To_Database_Reference (Commodity),
         Buyer      => Buyer,
         Seller     => Seller,
         Price      => Price,
         Quantity   => Quantity);

   end Execute_Transaction;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   function Historical_Mean_Price
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference)
      return Concorde.Money.Price_Type
   is
      pragma Unreferenced (Market);
      use Concorde.Money;
      Price : constant Price_Type :=
        Concorde.Commodities.Initial_Price (Commodity);
   begin
      return Price;
   end Historical_Mean_Price;

   -------------------------------
   -- Historical_Offer_Quantity --
   -------------------------------

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      use Concorde.Handles.Historical_Offer;
      Ref : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin
      return Quantity : Quantity_Type := Zero do
         for Historical_Offer of
           Select_Historical_Offer_Bounded_By_Time_Stamp
             (Market, Ref, Offer, From, To)
         loop
            Quantity := Quantity + Historical_Offer.Quantity;
         end loop;
      end return;
   end Historical_Offer_Quantity;

   -------------------------------
   -- Historical_Offer_Quantity --
   -------------------------------

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Handles.Offer_Type;
      use type Concorde.Money.Price_Type;
      use Concorde.Quantities;
      use Concorde.Handles.Historical_Offer;
      Ref : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin
      return Quantity : Quantity_Type := Zero do
         for Historical_Offer of
           Select_Historical_Offer_Bounded_By_Time_Stamp
             (Market, Ref, Offer, From, To)
         loop
            if (Offer = Concorde.Handles.Ask
                and then Price <= Historical_Offer.Price)
              or else (Offer = Concorde.Handles.Bid
                       and then Price >= Historical_Offer.Price)
            then
               Quantity := Quantity + Historical_Offer.Quantity;
            end if;
         end loop;
      end return;
   end Historical_Offer_Quantity;

   -------------------------------
   -- Historical_Offer_Quantity --
   -------------------------------

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Calendar.Time;
      Now : constant Concorde.Calendar.Time :=
        Concorde.Calendar.Clock;
   begin
      return Historical_Offer_Quantity
        (Market, Commodity, Offer, Now - Since, Now);
   end Historical_Offer_Quantity;

   function Historical_Offer_Quantity
     (Market    : Concorde_Market;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Calendar.Time;
      Now : constant Concorde.Calendar.Time :=
        Concorde.Calendar.Clock;
   begin
      return Historical_Offer_Quantity
        (Market, Commodity, Offer, Price, Now - Since, Now);
   end Historical_Offer_Quantity;

   ----------------
   -- Log_Market --
   ----------------

   procedure Log_Market
     (Market  : Concorde_Market;
      Message : String)
   is
   begin
      Concorde.Logging.Log
        (Actor    =>
           "Market" & Concorde.Handles.To_String (Market),
         Location =>
           Concorde.Worlds.Name (Concorde.Handles.Market.Get (Market).World),
         Category => "",
         Message  => Message);
   end Log_Market;

   --------------------------
   -- Previous_Agent_Offer --
   --------------------------

   function Previous_Agent_Offer
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Handles.Historical_Offer;
      Ref : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin
      return Quantity : Concorde.Quantities.Quantity_Type :=
        Concorde.Quantities.Zero
      do
         for Hist_Offer of
           Select_Reverse_Agent_Offer_Bounded_By_Time_Offset
             (Market, Agent, Ref, Offer, 0.0, Real'Last)
         loop
            Quantity := Hist_Offer.Quantity;
            exit;
         end loop;
      end return;
   end Previous_Agent_Offer;

   --------------------------------
   -- Previous_Agent_Offer_Price --
   --------------------------------

   function Previous_Agent_Offer_Price
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Money.Price_Type
   is
      use Concorde.Handles.Historical_Offer;
      Ref : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
   begin
      return Price : Concorde.Money.Price_Type :=
        Concorde.Money.Zero
      do
         for Hist_Offer of
           Select_Reverse_Agent_Offer_Bounded_By_Time_Offset
             (Market, Agent, Ref, Offer, 0.0, Real'Last)
         loop
            Price := Hist_Offer.Price;
            exit;
         end loop;
      end return;
   end Previous_Agent_Offer_Price;

   ---------------------------
   -- Remaining_Agent_Offer --
   ---------------------------

   function Remaining_Agent_Offer
     (Market    : Concorde_Market;
      Agent     : Concorde.Handles.Agent_Reference;
      Commodity : Concorde.Commodities.Commodity_Reference;
      Offer     : Concorde.Handles.Offer_Type)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Handles.Offer_Type;
      Ref  : constant Concorde.Handles.Commodity.Commodity_Class :=
        Concorde.Commodities.To_Database_Reference (Commodity);
      Item : constant Concorde.Handles.Market_Offer.Market_Offer_Type :=
        Concorde.Handles.Market_Offer.Get_By_Market_Offer
          (Market, Agent, Ref);
   begin
      if Item.Has_Element and then Item.Offer = Offer then
         return Item.Quantity;
      else
         return Concorde.Quantities.Zero;
      end if;
   end Remaining_Agent_Offer;

end Concorde.Markets;
