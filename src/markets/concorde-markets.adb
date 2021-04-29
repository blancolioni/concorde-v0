with WL.String_Maps;

with Concorde.Agents;
with Concorde.Logging;
with Concorde.Pops;
with Concorde.Stock;

with Concorde.Handles.Employer;
with Concorde.Handles.Pop;

package body Concorde.Markets is

   package Market_Maps is
     new WL.String_Maps (Concorde_Market);

   Market_Map : Market_Maps.Map;

   function To_Agora
     (Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Agora.Commodities.Commodity_Interface'Class
   is (Agora_Commodity'
         (Handle => Commodity.To_Commodity_Handle));

   function To_Agora
     (Agent : Concorde.Handles.Agent.Agent_Class)
      return Agora.Agents.Agent_Interface'Class
   is (Agora_Agent'
         (Handle => Agent.To_Agent_Handle));

   function To_Agora
     (Offer : Concorde.Db.Offer_Type)
      return Agora.Offer_Type
   is (case Offer is
          when Concorde.Db.Ask => Agora.Ask,
          when Concorde.Db.Bid => Agora.Bid);

   function To_Agora
     (Money : Concorde.Money.Money_Type)
      return Agora.Money_Type
   is (Agora.Money_Type (Concorde.Money.To_Real (Money)));

   function To_Agora
     (Price : Concorde.Money.Price_Type)
      return Agora.Price_Type
   is (Agora.Price_Type (Concorde.Money.To_Real (Price)));

   function To_Agora
     (Quantity : Concorde.Quantities.Quantity_Type)
      return Agora.Quantity_Type
   is (Agora.Quantity_Type (Concorde.Quantities.To_Real (Quantity)));

   function To_Agora
     (Clock : Concorde.Calendar.Time)
      return Agora.Sequence_Type
   is (Agora.Sequence_Type (Concorde.Calendar.To_Real (Clock)));

   function To_Concorde
     (Commodity : Agora.Commodities.Commodity_Interface'Class)
      return Concorde.Handles.Commodity.Commodity_Class
   is (Agora_Commodity'Class (Commodity).Handle);

   function To_Concorde
     (Money : Agora.Money_Type)
      return Concorde.Money.Money_Type
   is (Concorde.Money.To_Money (Real (Money)));

   function To_Concorde
     (Price : Agora.Price_Type)
      return Concorde.Money.Price_Type
   is (Concorde.Money.To_Price (Real (Price)));

   function To_Concorde
     (Quantity : Agora.Quantity_Type)
      return Concorde.Quantities.Quantity_Type
   is (Concorde.Quantities.To_Quantity (Real (Quantity)));

   ------------------
   -- Create_Offer --
   ------------------

   procedure Create_Offer
     (Market    : Concorde_Market'Class;
      Agent     : Concorde.Handles.Agent.Agent_Class;
      Offer     : Concorde.Db.Offer_Type;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Price     : Concorde.Money.Price_Type)
   is
   begin

      Concorde.Logging.Log
        (Actor    => Concorde.Agents.Describe (Agent),
         Location => Market.Db_Market.World.Name,
         Category => "offer",
         Message  =>
           (case Offer is
               when Concorde.Db.Ask => "asks",
               when Concorde.Db.Bid => "bids")
         & " "
         & Concorde.Money.Show (Concorde.Money.Total (Price, Quantity))
         & " for "
         & Concorde.Quantities.Show (Quantity)
         & " "
         & Commodity.Tag
         & " ("
         & Concorde.Money.Show (Price)
         & " each)");

      Market.Agora_Market.Create_Offer
        (Sequence  => To_Agora (Concorde.Calendar.Clock),
         Agent     => To_Agora (Agent),
         Offer     => To_Agora (Offer),
         Commodity => To_Agora (Commodity),
         Quantity  => To_Agora (Quantity),
         Price     => To_Agora (Price));
   end Create_Offer;

   -------------------
   -- Current_Price --
   -------------------

   function Current_Price
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Price_Type
   is
   begin
      return To_Concorde
        (Market.Agora_Market.Current_Price
           (To_Agora (Commodity),
            To_Agora (Offer),
            To_Agora (Quantity)));
   end Current_Price;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return To_Concorde
        (Market.Agora_Market.Current_Quantity
           (To_Agora (Commodity), To_Agora (Offer), Agora.Money_Type'Last));
   end Current_Quantity;

   ----------------------
   -- Current_Quantity --
   ----------------------

   function Current_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Cash      : Concorde.Money.Money_Type)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return To_Concorde
        (Market.Agora_Market.Current_Quantity
           (To_Agora (Commodity), To_Agora (Offer), To_Agora (Cash)));
   end Current_Quantity;

   -------------------
   -- Current_Value --
   -------------------

   function Current_Value
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Quantity  : Concorde.Quantities.Quantity_Type)
      return Concorde.Money.Money_Type
   is
   begin
      return To_Concorde
        (Market.Agora_Market.Current_Value
           (To_Agora (Commodity), To_Agora (Offer), To_Agora (Quantity)));
   end Current_Value;

   ---------------------------
   -- Historical_Mean_Price --
   ---------------------------

   function Historical_Mean_Price
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class)
      return Concorde.Money.Price_Type
   is
   begin
      return To_Concorde
        (Market.Agora_Market.Historical_Mean_Price
           (To_Agora (Commodity),
            Agora.Sequence_Type'First,
            Agora.Sequence_Type'Last));
   end Historical_Mean_Price;

   -------------------------------
   -- Historical_Offer_Quantity --
   -------------------------------

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return To_Concorde
        (Market.Agora_Market.Historical_Offer_Quantity
           (To_Agora (Commodity), To_Agora (Offer),
            (case Offer is
                when Concorde.Db.Ask => Agora.Price_Type'Last,
                when Concorde.Db.Bid => Agora.Price_Type'First),
            Agora.Sequence_Type (Concorde.Calendar.To_Real (From)),
            Agora.Sequence_Type (Concorde.Calendar.To_Real (To))));
   end Historical_Offer_Quantity;

   -------------------------------
   -- Historical_Offer_Quantity --
   -------------------------------

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      From, To  : Concorde.Calendar.Time)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return To_Concorde
        (Market.Agora_Market.Historical_Offer_Quantity
           (To_Agora (Commodity), To_Agora (Offer), To_Agora (Price),
            Agora.Sequence_Type (Concorde.Calendar.To_Real (From)),
            Agora.Sequence_Type (Concorde.Calendar.To_Real (To))));
   end Historical_Offer_Quantity;

   -------------------------------
   -- Historical_Offer_Quantity --
   -------------------------------

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Calendar.Time;
      Clock : constant Concorde.Calendar.Time := Concorde.Calendar.Clock;
   begin
      return Market.Historical_Offer_Quantity
        (Commodity, Offer, Clock - Since, Clock);
   end Historical_Offer_Quantity;

   -------------------------------
   -- Historical_Offer_Quantity --
   -------------------------------

   function Historical_Offer_Quantity
     (Market    : Concorde_Market'Class;
      Commodity : Concorde.Handles.Commodity.Commodity_Class;
      Offer     : Concorde.Db.Offer_Type;
      Price     : Concorde.Money.Price_Type;
      Since     : Concorde_Duration)
      return Concorde.Quantities.Quantity_Type
   is
      use type Concorde.Calendar.Time;
      Clock : constant Concorde.Calendar.Time := Concorde.Calendar.Clock;
   begin
      return Market.Historical_Offer_Quantity
        (Commodity, Offer, Price, Clock - Since, Clock);
   end Historical_Offer_Quantity;

   ----------
   -- Load --
   ----------

   procedure Load
     (Market : in out Concorde_Market'Class;
      Db     :        Concorde.Handles.Market.Market_Class)
   is
   begin
      Market.Agora_Market := new Agora.Markets.Agora_Market_Type;
      Market.Agora_Market.Initialize;
      Market.Db_Market    := Db.To_Market_Handle;
   end Load;

   ------------------
   -- Load_Markets --
   ------------------

   procedure Load_Markets is
   begin
      for Market of Concorde.Handles.Market.Scan_By_World loop
         declare
            M : Concorde_Market;
         begin
            M.Load (Market);
            Market_Map.Insert (Market.World.Identifier, M);
         end;
      end loop;
   end Load_Markets;

   ------------
   -- On_Buy --
   ------------

   overriding procedure On_Buy
     (Agent     : Agora_Agent;
      Seller    : Agora.Agents.Agent_Interface'Class;
      Commodity : Agora.Commodities.Commodity_Interface'Class;
      Quantity  : Agora.Quantity_Type;
      Price     : Agora.Price_Type)
   is
      Q : constant Concorde.Quantities.Quantity_Type :=
            To_Concorde (Quantity);
      P : constant Concorde.Money.Price_Type :=
            To_Concorde (Price);
      M : constant Concorde.Money.Money_Type :=
            Concorde.Money.Total (P, Q);
   begin
      Concorde.Logging.Log
        (Concorde.Agents.Describe (Agent.Handle),
         "bought " & Concorde.Quantities.Show (Q)
         & " " & To_Concorde (Commodity).Tag
         & " for " & Concorde.Money.Show (P)
         & " ea (total " & Concorde.Money.Show (M) & ")");

      Concorde.Agents.Remove_Cash
        (Agent => Agent.Handle,
         Cash  => M,
         Tag   => "bought");
      Concorde.Stock.Add_Stock
        (To       => Agent.Handle,
         Item     => To_Concorde (Commodity),
         Quantity => Q,
         Value    => M);
   end On_Buy;

   -------------
   -- On_Sell --
   -------------

   overriding procedure On_Sell
     (Agent     : Agora_Agent;
      Buyer     : Agora.Agents.Agent_Interface'Class;
      Commodity : Agora.Commodities.Commodity_Interface'Class;
      Quantity  : Agora.Quantity_Type;
      Price     : Agora.Price_Type)
   is
      Q : constant Concorde.Quantities.Quantity_Type :=
            To_Concorde (Quantity);
      P : constant Concorde.Money.Price_Type :=
            To_Concorde (Price);
      M : constant Concorde.Money.Money_Type :=
            Concorde.Money.Total (P, Q);
   begin
      Concorde.Logging.Log
        (Concorde.Agents.Describe (Agent.Handle),
         "sold " & Concorde.Quantities.Show (Q)
         & " " & To_Concorde (Commodity).Tag
         & " for " & Concorde.Money.Show (P)
         & " ea (total " & Concorde.Money.Show (M) & ")");

      Concorde.Agents.Add_Cash
        (Agent => Agent.Handle,
         Cash  => M,
         Tag   => "sold");

      Concorde.Stock.Remove_Stock
        (From     => Agent.Handle,
         Item     => To_Concorde (Commodity),
         Quantity => Q);

      declare
         use type Concorde.Db.Record_Type;
      begin
         if To_Concorde (Commodity).Top_Record = Concorde.Db.R_Pop_Group then
            Concorde.Pops.On_Employment
              (Pop      =>
                 Concorde.Handles.Pop.Get_From_Agent (Agent.Handle),
               Employer =>
                 Concorde.Handles.Employer.Get_From_Agent
                   (Agora_Agent (Buyer).Handle),
               Quantity => Q,
               Salary   => P);
         end if;
      end;

   end On_Sell;

   ----------
   -- Save --
   ----------

   procedure Save (Market : Concorde_Market'Class) is
   begin
      null;
   end Save;

   ------------------
   -- World_Market --
   ------------------

   function World_Market
     (World : Concorde.Handles.World.World_Class)
      return Concorde_Market
   is
   begin
      return Market_Map (World.Identifier);
   end World_Market;

end Concorde.Markets;
