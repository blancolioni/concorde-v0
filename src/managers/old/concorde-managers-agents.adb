with Ada.Strings.Fixed;

with Concorde.Logging;
with Concorde.Logs;
--  with Concorde.Real_Images;

with Concorde.Agents;
with Concorde.Markets;
with Concorde.Stock;
with Concorde.Worlds;

with Concorde.Db.Ask_Offer;
with Concorde.Db.Bid_Offer;
with Concorde.Db.Commodity;
with Concorde.Db.Historical_Offer;
with Concorde.Db.Historical_Stock;
with Concorde.Db.Stock_Price;
with Concorde.Db.Transaction;

package body Concorde.Managers.Agents is

   Supply_Field : constant := 1;
   Demand_Field : constant := 2;
   Traded_Field : constant := 3;
   Price_Field  : constant := 4;
   Last_Field   : constant := Price_Field;

   subtype Market_Log_Field_Index is Positive range 1 .. Last_Field;

   type Market_State_Log is
     new Concorde.Logs.Log_Interface with
      record
         Market    : Concorde.Db.Market_Reference;
         Commodity : Concorde.Db.Commodity_Reference;
         Supply    : Concorde.Quantities.Quantity_Type;
         Demand    : Concorde.Quantities.Quantity_Type;
         Traded    : Concorde.Quantities.Quantity_Type;
         Price     : Concorde.Money.Price_Type;
      end record;

   overriding function Path
     (Log : Market_State_Log)
      return String;

   overriding function Field_Count
     (Log : Market_State_Log)
      return Natural;

   overriding function Heading
     (Log   : Market_State_Log;
      Index : Positive)
      return String;

   overriding function Value
     (Log   : Market_State_Log;
      Index : Positive)
      return String;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : not null access Root_Agent_Manager)
   is
      M : Root_Agent_Manager'Class renames
            Root_Agent_Manager'Class (Manager.all);
   begin
      Manager.Log
        ("activating: cash = "
         & Concorde.Money.Show
           (Concorde.Agents.Cash (M.Account)));

      Concorde.Markets.Reset_Offers (Manager.Market, Manager.Agent);
      M.Create_Market_Offers;
      M.Execute_Agent_Tasks;
      Manager.Set_Next_Update_Delay
        (M.Next_Sleep_Duration);
   end Activate;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Value     : Concorde.Money.Money_Type)
   is
   begin
      Concorde.Stock.Add_Stock
        (Manager.Has_Stock, Commodity, Quantity, Value);
   end Add_Stock;

   ------------------------
   -- Calculate_Capacity --
   ------------------------

   function Calculate_Capacity
     (Manager : Root_Agent_Manager;
      Stock   : Concorde.Commodities.Stock_Type)
      return Unit_Real
   is
      Capacity : Unit_Real := 1.0;

      procedure Limit_Capacity
        (Commodity : Concorde.Db.Commodity_Reference;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type);

      --------------------
      -- Limit_Capacity --
      --------------------

      procedure Limit_Capacity
        (Commodity : Concorde.Db.Commodity_Reference;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type)
      is
         pragma Unreferenced (Value);
         use Concorde.Quantities;
         Required  : constant Quantity_Type := Quantity;
         Have      : constant Quantity_Type :=
                       Stock.Get_Quantity (Commodity);
         Limit     : constant Non_Negative_Real :=
                       To_Real (Have) / To_Real (Required);
      begin
         if Limit < Capacity then
            Capacity := Limit * 0.9;
         end if;
      end Limit_Capacity;

      Required_Stock : Concorde.Commodities.Stock_Type;

   begin
      Root_Agent_Manager'Class (Manager).Get_Required_Stock (Required_Stock);

      Required_Stock.Iterate (Limit_Capacity'Access);

      return Capacity;

   end Calculate_Capacity;

   ----------
   -- Cash --
   ----------

   function Cash
     (Manager : Root_Agent_Manager'Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Concorde.Agents.Cash (Manager.Account);
   end Cash;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   procedure Create_Market_Offers
     (Manager : in out Root_Agent_Manager)
   is
      use Concorde.Commodities;
      use Concorde.Money;
      use Concorde.Quantities;
      M         : Root_Agent_Manager'Class renames
                    Root_Agent_Manager'Class (Manager);
      Have      : Concorde.Commodities.Stock_Type;
      Required  : Concorde.Commodities.Stock_Type;
      Missing   : Concorde.Commodities.Stock_Type;
--        Available : Concorde.Commodities.Stock_Type;
--        Cost      : Concorde.Money.Money_Type;
--        Cash      : constant Concorde.Money.Money_Type := Manager.Cash;

      procedure Place_Bid
        (Commodity : Concorde.Db.Commodity_Reference;
         Quantity  : Quantity_Type;
         Value     : Money_Type);

      ---------------
      -- Place_Bid --
      ---------------

      procedure Place_Bid
        (Commodity : Concorde.Db.Commodity_Reference;
         Quantity  : Quantity_Type;
         Value     : Money_Type)
      is
         pragma Unreferenced (Value);
         Price : constant Concorde.Money.Price_Type :=
                   Manager.Minimum_Bid_Price (Commodity, Quantity);
      begin
         Manager.Place_Bid (Commodity, Quantity, Price);
      end Place_Bid;

   begin
      Concorde.Logging.Log
        (Actor    => M.Managed_Object_Id,
         Location => Concorde.Worlds.Name (Manager.World),
         Category => "manager",
         Message  => "create market offers");
      Manager.Current_Stock (Have);
      Manager.Capacity := 1.0;
      M.Get_Desired_Stock (Required);
      Missing := Concorde.Commodities.Missing (Have, Required);

      if Missing.Total_Quantity > Zero then
--           Manager.Try_Bids (Missing, Available);
--           Cost := Available.Total_Value;
--
--           if Cost > Cash then
--              declare
--                 Base : constant Unit_Real :=
--                          M.Calculate_Capacity (Have);
--              begin
--                 Manager.Capacity :=
--                   Base +
--                     (Manager.Capacity - Base)
--                   * To_Real (Cash) / To_Real (Cost);
--              end;
--           end if;
--
--           Concorde.Logging.Log
--             (Actor    => "Agent" & Concorde.Db.To_String (Manager.Agent),
--              Location => Concorde.Worlds.Name (Manager.World),
--              Category => "manager",
--              Message  =>
--                "available commodities "
--              & Concorde.Quantities.Show (Available.Total_Quantity)
--              & "; cost of missing commodities: "
--              & Concorde.Money.Show (Cost)
--              & "; cash "
--              & Concorde.Money.Show (Cash)
--              & "; estimated capacity: "
--              & Concorde.Real_Images.Approximate_Image (Manager.Capacity));
--
--           Missing := Available.Missing (Required);

         Missing.Iterate (Place_Bid'Access);
      end if;
   end Create_Market_Offers;

   -------------------------------
   -- Current_Agent_Stock_Price --
   -------------------------------

   function Current_Agent_Stock_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type
   is
      Rec : constant Concorde.Db.Stock_Price.Stock_Price_Type :=
              Concorde.Db.Stock_Price.Get_By_Stock_Price
                (Manager.Agent, Commodity);
   begin
      if Rec.Has_Element then
         return Rec.Price;
      else
         return Concorde.Commodities.Initial_Price (Commodity);
      end if;
   end Current_Agent_Stock_Price;

   ------------------------------
   -- Current_Market_Ask_Price --
   ------------------------------

   function Current_Market_Ask_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Markets.Current_Ask_Price
        (Manager.Market, Commodity);
   end Current_Market_Ask_Price;

   ---------------------------------
   -- Current_Market_Ask_Quantity --
   ---------------------------------

   function Current_Market_Ask_Quantity
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      Quantity : Quantity_Type := Zero;
   begin
      for Ask of
        Concorde.Db.Ask_Offer.Select_Market_Priority_Bounded_By_Priority
          (Manager.Market, Commodity, 0.0, Real'Last)
      loop
         Quantity := Quantity + Ask.Quantity;
      end loop;
      return Quantity;
   end Current_Market_Ask_Quantity;

   ------------------------------
   -- Current_Market_Bid_Price --
   ------------------------------

   function Current_Market_Bid_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Markets.Current_Bid_Price
        (Manager.Market, Commodity);
   end Current_Market_Bid_Price;

   ---------------------------------
   -- Current_Market_Bid_Quantity --
   ---------------------------------

   function Current_Market_Bid_Quantity
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      Quantity : Quantity_Type := Zero;
   begin
      for Ask of
        Concorde.Db.Bid_Offer.Select_Market_Priority_Bounded_By_Priority
          (Manager.Market, Commodity, 0.0, Real'Last)
      loop
         Quantity := Quantity + Ask.Quantity;
      end loop;
      return Quantity;
   end Current_Market_Bid_Quantity;

   -------------------
   -- Current_Stock --
   -------------------

   procedure Current_Stock
     (Manager : Root_Agent_Manager'Class;
      Stock   : out Concorde.Commodities.Stock_Type)
   is
   begin
      Stock.Load (Manager.Has_Stock);
   end Current_Stock;

   -------------------
   -- Current_Stock --
   -------------------

   function Current_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Stock.Get_Quantity (Manager.Has_Stock, Commodity);
   end Current_Stock;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Manager : Root_Agent_Manager'Class;
      Amount  : Concorde.Money.Money_Type)
   is
   begin
      Manager.Log ("earn " & Concorde.Money.Show (Amount));
      Concorde.Agents.Add_Cash (Manager.Account, Amount);
   end Earn;

   -----------------
   -- Field_Count --
   -----------------

   overriding function Field_Count
     (Log : Market_State_Log)
      return Natural
   is
      pragma Unreferenced (Log);
   begin
      return Last_Field;
   end Field_Count;

   -----------------------
   -- Get_Desired_Stock --
   -----------------------

   procedure Get_Desired_Stock
     (Manager  : Root_Agent_Manager;
      Stock    : in out Concorde.Commodities.Stock_Type)
   is
   begin
      Root_Agent_Manager'Class (Manager).Get_Required_Stock (Stock);
   end Get_Desired_Stock;

   -------------
   -- Heading --
   -------------

   overriding function Heading
     (Log   : Market_State_Log;
      Index : Positive)
      return String
   is
      pragma Unreferenced (Log);
   begin
      case Market_Log_Field_Index (Index) is
         when Supply_Field =>
            return "Supply";
         when Demand_Field =>
            return "Demand";
         when Traded_Field =>
            return "Traded";
         when Price_Field =>
            return "Price";
      end case;
   end Heading;

   ------------------------------
   -- Initialize_Agent_Manager --
   ------------------------------

   procedure Initialize_Agent_Manager
     (Manager : in out Root_Agent_Manager'Class;
      Agent   : Concorde.Db.Agent.Agent_Type;
      World   : Concorde.Db.World_Reference)
   is
   begin
      Manager.Agent := Agent.Get_Agent_Reference;
      Manager.Has_Stock := Agent.Get_Has_Stock_Reference;
      Manager.Market := Concorde.Worlds.Market (World);
      Manager.Account := Agent.Account;
      Manager.World := World;
   end Initialize_Agent_Manager;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager : Root_Agent_Manager'Class;
      Message : String)
   is
   begin
      Concorde.Agents.Log_Agent
        (Manager.Agent,
         Manager.Managed_Object_Id & ": "
         & Message);
   end Log;

   ----------------------
   -- Log_Market_State --
   ----------------------

   procedure Log_Market_State
     (Manager : Root_Agent_Manager'Class)
   is
      use Concorde.Calendar;
      Now : constant Time := Clock;
   begin
      for Commodity of Concorde.Db.Commodity.Scan_By_Tag loop
         declare
            use Concorde.Money, Concorde.Quantities;
            Traded : Quantity_Type := Zero;
            Value  : Money_Type := Zero;
            Supply : Quantity_Type := Zero;
            Demand : Quantity_Type := Zero;
         begin
            for Transaction of
              Concorde.Db.Transaction.Select_Transaction_Bounded_By_Time_Stamp
                (Manager.Market, Commodity.Get_Commodity_Reference,
                 Now - Days (1.0), Now)
            loop
               Traded := Traded + Transaction.Quantity;
               Value := Value
                 + Total (Transaction.Price, Transaction.Quantity);
            end loop;

            for Offer of
              Concorde.Db.Historical_Offer
                .Select_Historical_Offer_Bounded_By_Time_Stamp
                  (Manager.Market, Commodity.Get_Commodity_Reference,
                   Now - Days (1.0), Now)
            loop
               case Offer.Offer is
                  when Concorde.Db.Ask =>
                     Supply := Supply + Offer.Quantity;
                  when Concorde.Db.Bid =>
                     Demand := Demand + Offer.Quantity;
               end case;
            end loop;

            declare
               Log : constant Market_State_Log :=
                       Market_State_Log'
                         (Market    => Manager.Market,
                          Commodity => Commodity.Get_Commodity_Reference,
                          Supply    => Supply,
                          Demand    => Demand,
                          Traded    => Traded,
                          Price     => Price (Value, Traded));
            begin
               Log.Log;
            end;
         end;
      end loop;

   end Log_Market_State;

   -----------------------
   -- Minimum_Bid_Price --
   -----------------------

   function Minimum_Bid_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Markets.Minimum_Bid_Price
        (Manager.Market, Commodity, Quantity);
   end Minimum_Bid_Price;

   -------------------------
   -- Next_Sleep_Duration --
   -------------------------

   function Next_Sleep_Duration
     (Manager : Root_Agent_Manager)
      return Duration
   is
      pragma Unreferenced (Manager);
   begin
      return Concorde.Calendar.Days (1.0);
   end Next_Sleep_Duration;

   ----------
   -- Path --
   ----------

   overriding function Path
     (Log : Market_State_Log)
      return String
   is
   begin
      return "markets/"
        & Ada.Strings.Fixed.Trim (Concorde.Db.To_String (Log.Market),
                                  Ada.Strings.Both)
        & "/"
        & Concorde.Db.Commodity.Get (Log.Commodity).Tag;
   end Path;

   ---------
   -- Pay --
   ---------

   procedure Pay
     (Manager : Root_Agent_Manager'Class;
      Amount  : Concorde.Money.Money_Type)
   is
   begin
      Manager.Log ("pay " & Concorde.Money.Show (Amount));
      Concorde.Agents.Remove_Cash (Manager.Account, Amount);
   end Pay;

   ---------------
   -- Place_Ask --
   ---------------

   procedure Place_Ask
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Manager.Log
        ("ask " & Concorde.Money.Show (Price)
         & " for "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Concorde.Commodities.Local_Name (Commodity)
         & "; total "
         & Concorde.Money.Show
           (Concorde.Money.Total (Price, Quantity)));
      Concorde.Markets.Ask
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Account   => Manager.Account,
         Has_Stock => Manager.Has_Stock,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);
   end Place_Ask;

   ---------------
   -- Place_Bid --
   ---------------

   procedure Place_Bid
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin
      Manager.Log
        ("bid " & Concorde.Money.Show (Price)
         & " for "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Concorde.Commodities.Local_Name (Commodity)
         & "; total "
         & Concorde.Money.Show
           (Concorde.Money.Total (Price, Quantity)));

      Concorde.Markets.Bid
        (Market    => Manager.Market,
         Agent     => Manager.Agent,
         Account   => Manager.Account,
         Has_Stock => Manager.Has_Stock,
         Commodity => Commodity,
         Quantity  => Quantity,
         Price     => Price);
   end Place_Bid;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
   begin
      Concorde.Stock.Remove_Stock
        (Manager.Has_Stock, Commodity, Quantity);
   end Remove_Stock;

   ------------------------
   -- Scan_Current_Stock --
   ------------------------

   procedure Scan_Current_Stock
     (Manager   : Root_Agent_Manager'Class;
      Process   : not null access
        procedure (Item     : Concorde.Db.Commodity_Reference;
                   Quantity : Concorde.Quantities.Quantity_Type;
                   Value    : Concorde.Money.Money_Type))
   is
   begin
      Concorde.Stock.Scan_Stock
        (Has_Stock => Manager.Has_Stock,
         Process   => Process);
   end Scan_Current_Stock;

   ---------------------------
   -- Scan_Historical_Stock --
   ---------------------------

   procedure Scan_Historical_Stock
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Days      : Non_Negative_Real;
      Process   : not null access
        procedure (Date : Concorde.Calendar.Time;
                   Quantity : Concorde.Quantities.Quantity_Type))
   is
      use Concorde.Calendar;
      Now   : constant Time := Clock;
      Start : constant Time := Now - Concorde.Calendar.Days (Days);
   begin
      for Historical_Stock of
        Concorde.Db.Historical_Stock
          .Select_Historical_Stock_Bounded_By_Time_Stamp
            (Has_Stock         => Manager.Has_Stock,
             Commodity         => Commodity,
             Start_Time_Stamp  => Start,
             Finish_Time_Stamp => Now)
      loop
         Process (Historical_Stock.Time_Stamp,
                  Historical_Stock.Quantity);
      end loop;
   end Scan_Historical_Stock;

   ---------------------------
   -- Set_Agent_Stock_Price --
   ---------------------------

   procedure Set_Agent_Stock_Price
     (Manager   : Root_Agent_Manager'Class;
      Commodity : Concorde.Db.Commodity_Reference;
      Price     : Concorde.Money.Price_Type)
   is
      Rec : constant Concorde.Db.Stock_Price.Stock_Price_Type :=
              Concorde.Db.Stock_Price.Get_By_Stock_Price
                (Manager.Agent, Commodity);
   begin
      if Rec.Has_Element then
         Rec.Set_Price (Price);
      else
         Concorde.Db.Stock_Price.Create
           (Agent     => Manager.Agent,
            Commodity => Commodity,
            Price     => Price);
      end if;
   end Set_Agent_Stock_Price;

   --------------
   -- Try_Bids --
   --------------

   procedure Try_Bids
     (Manager   : Root_Agent_Manager'Class;
      Stock     : Concorde.Commodities.Stock_Type;
      Available : out Concorde.Commodities.Stock_Type)
   is
   begin
      Concorde.Markets.Try_Bid
        (Market    => Manager.Market,
         Wanted    => Stock,
         Available => Available);
   end Try_Bids;

   -----------
   -- Value --
   -----------

   overriding function Value
     (Log   : Market_State_Log;
      Index : Positive)
      return String
   is
   begin
      case Market_Log_Field_Index (Index) is
         when Supply_Field =>
            return Concorde.Quantities.Show (Log.Supply);
         when Demand_Field =>
            return Concorde.Quantities.Show (Log.Demand);
         when Traded_Field =>
            return Concorde.Quantities.Show (Log.Traded);
         when Price_Field =>
            return Concorde.Money.Image (Log.Price);
      end case;
   end Value;

   -----------
   -- World --
   -----------

   function World
     (Manager : Root_Agent_Manager'Class)
      return Concorde.Db.World_Reference
   is
   begin
      return Manager.World;
   end World;

end Concorde.Managers.Agents;
