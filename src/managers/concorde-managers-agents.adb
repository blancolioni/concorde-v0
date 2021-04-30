with WL.Random;

with Concorde.Agents;
with Concorde.Random;
with Concorde.Stock;

with Concorde.Handles.Lease_Contract;
with Concorde.Handles.Stock_Item;

with Concorde.Db;

package body Concorde.Managers.Agents is

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager_Type)
   is
      M : Root_Agent_Manager_Type'Class renames
            Root_Agent_Manager_Type'Class (Manager.all);
   begin
      Manager.Log
        ("activating: cash = "
         & Concorde.Money.Show (M.Account.Cash));

      M.On_Activation_Begin;

      if Manager.Update_Count mod Manager.Planning_Cycle = 0 then
         M.Create_Planning;
      end if;

      M.Pay_Daily_Costs;

      M.Set_Requirements;
      M.Set_Sale_Stock;

      M.Create_Bids;
      M.Create_Asks;

      M.Execute_Production;
      M.Execute_Consumption;

      M.On_Activation_End;

      if M.Update_Count = 0 then
         M.Update_Count :=
           WL.Random.Random_Number (1, Manager.Planning_Cycle);
         M.Set_Next_Update_Delay
           (Concorde.Calendar.Days (Concorde.Random.Unit_Random + 0.5));
      else
         M.Update_Count := M.Update_Count + 1;
         M.Set_Next_Update_Delay (Concorde.Calendar.Days (1));
      end if;

   end Activate;

   -------------
   -- Add_Ask --
   -------------

   procedure Add_Ask
     (Manager   : in out Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type)
   is
   begin
      Manager.Sell.Append
        (Stock_Item_Record'
           (Commodity => Commodity.To_Commodity_Handle,
            Quantity  => Quantity,
            Value     => Value));
   end Add_Ask;

   ---------------------
   -- Add_Requirement --
   ---------------------

   procedure Add_Requirement
     (Manager   : in out Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Necessary : Concorde.Quantities.Quantity_Type;
      Desired   : Concorde.Quantities.Quantity_Type)
   is
   begin
      Manager.Log
        (Commodity.Tag & ": necessary "
         & Concorde.Quantities.Show (Necessary)
         & "; desired "
         & Concorde.Quantities.Show (Desired));

      Manager.Necessary.Append
        (Stock_Item_Record'
           (Commodity => Commodity.To_Commodity_Handle,
            Quantity  => Necessary,
            Value     => Concorde.Money.Zero));
      Manager.Desired.Append
        (Stock_Item_Record'
           (Commodity => Commodity.To_Commodity_Handle,
            Quantity  => Desired,
            Value     => Concorde.Money.Zero));
   end Add_Requirement;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type)
   is
   begin
      Concorde.Stock.Add_Stock
        (To       => Manager.Has_Stock,
         Item     => Commodity,
         Quantity => Quantity,
         Value    => Value);
   end Add_Stock;

   ---------------
   -- Available --
   ---------------

   function Available
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Boolean
   is
      use type Concorde.Quantities.Quantity_Type;
   begin
      return Concorde.Markets.Historical_Offer_Quantity
        (Manager.Market, Commodity, Concorde.Db.Ask,
         Concorde.Calendar.Days (10))
        > Concorde.Quantities.Zero;
   end Available;

   ----------------
   -- Create_Ask --
   ----------------

   procedure Create_Ask
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Manager.Log
        ("ask: " & Commodity.Tag
         & ": " & Concorde.Quantities.Show (Quantity)
         & " "
         & " @ " & Concorde.Money.Show (Price)
         & "ea; total "
         & Concorde.Money.Show
           (Concorde.Money.Total (Price, Quantity)));

      Concorde.Markets.Create_Offer
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Offer     => Concorde.Db.Ask,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);

   end Create_Ask;

   -----------------
   -- Create_Asks --
   -----------------

   procedure Create_Asks
     (Manager : in out Root_Agent_Manager_Type)
   is
      use Concorde.Money, Concorde.Quantities;
   begin
      Manager.Log ("creating asks");

      for Item of Manager.Sell loop
         declare
            Min_Price : constant Price_Type :=
                          Price (Item.Value, Item.Quantity);
         begin
            Manager.Create_Ask
              (Commodity => Item.Commodity,
               Quantity  => Item.Quantity,
               Price     =>
                 Max
                   (Min_Price,
                    Manager.Current_Bid_Price
                      (Item.Commodity,
                       Item.Quantity)));
         end;
      end loop;

   end Create_Asks;

   ----------------
   -- Create_Bid --
   ----------------

   procedure Create_Bid
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Manager.Log
        ("bid: " & Commodity.Tag
         & ": " & Concorde.Quantities.Show (Quantity)
         & " @ " & Concorde.Money.Show (Price)
         & "ea; total "
         & Concorde.Money.Show
           (Concorde.Money.Total (Price, Quantity)));

      Concorde.Markets.Create_Offer
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Offer     => Concorde.Db.Bid,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);

   end Create_Bid;

   -----------------
   -- Create_Bids --
   -----------------

   procedure Create_Bids
     (Manager : in out Root_Agent_Manager_Type)
   is
      use Concorde.Money, Concorde.Quantities;
      Remaining      : Money_Type :=
                         Concorde.Agents.Limit_Cash (Manager.Account);
      Necessary_Cost : Money_Type := Zero;
      --  Desired_Cost   : Money_Type := Zero;
   begin
      Manager.Log ("creating bids; limit cash " & Show (Remaining));
      if Remaining <= Zero then
         return;
      end if;

      if not Manager.Necessary.Is_Empty then
         for Item of Manager.Necessary loop
            Necessary_Cost := Necessary_Cost
              + Total (Manager.Current_Ask_Price
                       (Item.Commodity, Item.Quantity),
                       Item.Quantity);
         end loop;

         declare
            Necessary_Factor : constant Unit_Real :=
                                 (if Necessary_Cost <= Remaining
                                  then 1.0
                                  else To_Real (Remaining)
                                  / To_Real (Necessary_Cost));
         begin
            for Item of Manager.Necessary loop
               declare
                  Quantity : constant Quantity_Type :=
                               Scale (Item.Quantity, Necessary_Factor);
                  Price    : constant Price_Type :=
                               Manager.Current_Ask_Price
                                 (Item.Commodity, Quantity);
                  Value    : constant Money_Type := Total (Price, Quantity);
               begin
                  Manager.Create_Bid
                    (Commodity => Item.Commodity,
                     Quantity  => Quantity,
                     Price     => Price);
                  Remaining := Remaining - Value;
               end;
            end loop;
         end;
      end if;
   end Create_Bids;

   -----------------------
   -- Current_Ask_Price --
   -----------------------

   function Current_Ask_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Markets.Current_Price
        (Manager.Market, Commodity, Concorde.Db.Ask, Quantity);
   end Current_Ask_Price;

   --------------------------
   -- Current_Ask_Quantity --
   --------------------------

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Current_Quantity
        (Manager.Market, Commodity, Concorde.Db.Ask);
   end Current_Ask_Quantity;

   --------------------------
   -- Current_Ask_Quantity --
   --------------------------

   function Current_Ask_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Bid_Total : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Current_Quantity
        (Manager.Market, Commodity, Concorde.Db.Ask, Bid_Total);
   end Current_Ask_Quantity;

   -----------------------
   -- Current_Bid_Price --
   -----------------------

   function Current_Bid_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Markets.Current_Price
        (Manager.Market, Commodity, Concorde.Db.Bid, Quantity);
   end Current_Bid_Price;

   --------------------------
   -- Current_Bid_Quantity --
   --------------------------

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Current_Quantity
        (Manager.Market, Commodity, Concorde.Db.Bid);
   end Current_Bid_Quantity;

   --------------------------
   -- Current_Bid_Quantity --
   --------------------------

   function Current_Bid_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Ask_Total : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Current_Quantity
        (Manager.Market, Commodity, Concorde.Db.Bid, Ask_Total);
   end Current_Bid_Quantity;

   ----------------------
   -- Current_Buy_Cost --
   ----------------------

   function Current_Buy_Cost
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type
   is
   begin
      return Concorde.Markets.Current_Value
        (Manager.Market, Commodity, Concorde.Db.Ask, Quantity);
   end Current_Buy_Cost;

   ------------------
   -- Current_Cash --
   ------------------

   function Current_Cash
     (Manager : Root_Agent_Manager_Type'Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Manager.Account.Cash;
   end Current_Cash;

   -----------------------
   -- Current_Sell_Earn --
   -----------------------

   function Current_Sell_Earn
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type
   is
   begin
      return Concorde.Markets.Current_Value
        (Manager.Market, Commodity, Concorde.Db.Bid, Quantity);
   end Current_Sell_Earn;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Concorde.Money.Money_Type;
      Tag     : String)
   is
   begin
      Concorde.Agents.Add_Cash
        (Manager.Account, Amount, Tag);
   end Earn;

   ---------------
   -- Has_Stock --
   ---------------

   function Has_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Boolean
   is
      use type Concorde.Quantities.Quantity_Type;
   begin
      return Concorde.Stock.Get_Quantity (Manager.Has_Stock, Commodity)
        > Concorde.Quantities.Zero;
   end Has_Stock;

   -----------------------
   -- Historical_Demand --
   -----------------------

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Concorde.Db.Bid,
         Since     => Since);
   end Historical_Demand;

   -----------------------
   -- Historical_Demand --
   -----------------------

   function Historical_Demand
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Min_Price : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Concorde.Db.Bid,
         Price     => Min_Price,
         Since     => Since);
   end Historical_Demand;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   function Historical_Mean_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Markets.Historical_Mean_Price
        (Manager.Market, Commodity);
   end Historical_Mean_Price;

   -----------------------
   -- Historical_Supply --
   -----------------------

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Concorde.Db.Ask,
         Since     => Since);
   end Historical_Supply;

   -----------------------
   -- Historical_Supply --
   -----------------------

   function Historical_Supply
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Max_Price : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Markets.Historical_Offer_Quantity
        (Market    => Manager.Market,
         Commodity => Commodity,
         Offer     => Concorde.Db.Ask,
         Price     => Max_Price,
         Since     => Since);
   end Historical_Supply;

   ------------------------------
   -- Initialize_Agent_Manager --
   ------------------------------

   procedure Initialize_Agent_Manager
     (Manager           : in out Root_Agent_Manager_Type'Class;
      Agent             : Concorde.Handles.Agent.Agent_Class;
      Market            : Concorde.Markets.Concorde_Market;
      Planning_Cycle    : Positive)
   is
   begin
      Manager.Agent := Agent.To_Agent_Handle;
      Manager.Has_Stock := Agent.To_Has_Stock_Handle;
      Manager.Account := Agent.Account.To_Account_Handle;
      Manager.Market := Market;
      Manager.Planning_Cycle := Planning_Cycle;
   end Initialize_Agent_Manager;

   ---------------
   -- Last_Earn --
   ---------------

   function Last_Earn
     (Manager : Root_Agent_Manager_Type'Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Manager.Last_Earn;
   end Last_Earn;

   ----------------
   -- Last_Spend --
   ----------------

   function Last_Spend
     (Manager : Root_Agent_Manager_Type'Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Manager.Last_Spend;
   end Last_Spend;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager : Root_Agent_Manager_Type'Class;
      Message : String)
   is
   begin
      Concorde.Agents.Log_Agent
        (Agent   => Manager.Agent,
         Context => Manager.Identifier,
         Message => Message);
   end Log;

   -------------------------
   -- On_Activation_Begin --
   -------------------------

   procedure On_Activation_Begin
     (Manager : in out Root_Agent_Manager_Type)
   is
      use type Concorde.Calendar.Time;
   begin
      for Contract of
        Concorde.Handles.Lease_Contract.Select_By_Tenant (Manager.Agent)
      loop
         if Contract.Expires > Concorde.Calendar.Clock then
            Manager.Log ("pay " & Concorde.Money.Show (Contract.Daily_Rent)
                         & " to "
                         & Concorde.Agents.Describe (Contract.Owner)
                         & " for "
                         & Contract.Commodity.Tag);
            Manager.Spend (Contract.Daily_Rent, "rent");
            Concorde.Agents.Add_Cash
              (Contract.Owner, Contract.Daily_Rent,
               "rent");
         end if;
      end loop;
      Manager.Last_Earn := Manager.Account.Earn;
      Manager.Last_Spend := Manager.Account.Spend;
      Manager.Reset_Cashflow;
      Manager.Log ("last period earned "
                   & Concorde.Money.Show (Manager.Last_Earn)
                   & " and spent "
                   & Concorde.Money.Show (Manager.Last_Spend));

      Manager.Necessary.Clear;
      Manager.Desired.Clear;

   end On_Activation_Begin;

   -----------------------
   -- On_Activation_End --
   -----------------------

   procedure On_Activation_End
     (Manager : in out Root_Agent_Manager_Type)
   is null;

   ------------------
   -- Previous_Ask --
   ------------------

   --  function Previous_Ask
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type
   --  is
   --  begin
   --     return Concorde.Markets.Previous_Agent_Offer
   --       (Manager.Market, Manager.Agent, Commodity, Concorde.Db.Ask);
   --  end Previous_Ask;

   ------------------------
   -- Previous_Ask_Price --
   ------------------------

   --  function Previous_Ask_Price
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Money.Price_Type
   --  is
   --  begin
   --     return Concorde.Markets.Previous_Agent_Offer_Price
   --       (Manager.Market, Manager.Agent, Commodity, Concorde.Db.Ask);
   --  end Previous_Ask_Price;

   ------------------
   -- Previous_Bid --
   ------------------

   --  function Previous_Bid
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type
   --  is
   --  begin
   --     return Concorde.Markets.Previous_Agent_Offer
   --       (Manager.Market, Manager.Agent, Commodity, Concorde.Db.Ask);
   --  end Previous_Bid;

   -------------------
   -- Remaining_Ask --
   -------------------

   --  function Remaining_Ask
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type
   --  is
   --  begin
   --     return Concorde.Markets.Remaining_Agent_Offer
   --       (Manager.Market, Manager.Agent, Commodity, Concorde.Db.Ask);
   --  end Remaining_Ask;

   -------------------
   -- Remaining_Bid --
   -------------------

   --  function Remaining_Bid
   --    (Manager   : Root_Agent_Manager_Type'Class;
   --     Commodity : Concorde.Handles.Commodity.Commodity_Class)
   --     return Concorde.Quantities.Quantity_Type
   --  is
   --  begin
   --     return Concorde.Markets.Remaining_Agent_Offer
   --       (Manager.Market, Manager.Agent, Commodity, Concorde.Db.Ask);
   --  end Remaining_Bid;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
   begin
      Concorde.Stock.Remove_Stock
        (Manager.Has_Stock, Commodity, Quantity);
   end Remove_Stock;

   --------------------
   -- Reset_Cashflow --
   --------------------

   procedure Reset_Cashflow
     (Manager : Root_Agent_Manager_Type'Class)
   is
   begin
      Concorde.Handles.Agent.Update_Agent (Manager.Agent)
        .Set_Last_Earn (Manager.Last_Earn)
        .Set_Last_Spend (Manager.Last_Spend)
        .Done;
      Concorde.Handles.Account.Update_Account (Manager.Account)
        .Set_Earn (Concorde.Money.Zero)
        .Set_Spend (Concorde.Money.Zero)
        .Done;
   end Reset_Cashflow;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Manager : Root_Agent_Manager_Type'Class;
      Process : not null access
        procedure (Commodity : Concorde.Handles.Commodity.Commodity_Class;
                   Quantity  : Concorde.Quantities.Quantity_Type;
                   Value     : Concorde.Money.Money_Type))
   is
      use Concorde.Quantities;

      List : Stock_Item_Lists.List;

   begin
      for Stock_Item of
        Concorde.Handles.Stock_Item.Select_By_Has_Stock
          (Manager.Has_Stock)
      loop
         if Stock_Item.Quantity > Zero then
            List.Append
              ((Stock_Item.Commodity.To_Commodity_Handle,
               Stock_Item.Quantity, Stock_Item.Value));
         end if;
      end loop;

      for Stock_Item of List loop
         Process
           (Stock_Item.Commodity,
            Stock_Item.Quantity, Stock_Item.Value);
      end loop;
   end Scan_Stock;

   -----------
   -- Spend --
   -----------

   procedure Spend
     (Manager : Root_Agent_Manager_Type'Class;
      Amount  : Concorde.Money.Money_Type;
      Tag     : String)
   is
   begin
      Concorde.Agents.Remove_Cash
        (Manager.Account, Amount, Tag);
   end Spend;

   -----------------
   -- Stock_Price --
   -----------------

   function Stock_Price
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Stock.Get_Price_Per_Item
        (Manager.Has_Stock, Commodity);
   end Stock_Price;

   --------------------
   -- Stock_Quantity --
   --------------------

   function Stock_Quantity
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Stock.Get_Quantity (Manager.Has_Stock, Commodity);
   end Stock_Quantity;

   -----------------
   -- Stock_Value --
   -----------------

   function Stock_Value
     (Manager   : Root_Agent_Manager_Type'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Concorde.Stock.Get_Value (Manager.Has_Stock, Commodity);
   end Stock_Value;

end Concorde.Managers.Agents;
