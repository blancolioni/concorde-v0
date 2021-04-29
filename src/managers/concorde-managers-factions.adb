with Concorde.Commodities;
with Concorde.Quantities;
with Concorde.Money;

with Concorde.Agents;
with Concorde.Managers.Agents;

with Concorde.Handles.Agent;
with Concorde.Handles.Company;
with Concorde.Handles.Faction;
with Concorde.Handles.Market;
with Concorde.Handles.Shareholder;

package body Concorde.Managers.Factions is

   type Root_Faction_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Faction : Concorde.Handles.Faction.Faction_Handle;
      end record;

   overriding function Identifier
     (Manager : Root_Faction_Manager)
      return String
   is ("faction" & Concorde.Handles.To_String (Manager.Faction) & " manager");

   overriding procedure Create_Planning
     (Manager : in out Root_Faction_Manager)
   is null;

   overriding procedure Create_Bids
     (Manager : in out Root_Faction_Manager);

   overriding procedure Execute_Consumption
     (Manager : in out Root_Faction_Manager)
   is null;

   overriding procedure Execute_Production
     (Manager : in out Root_Faction_Manager)
   is null;

   type Faction_Company_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Faction : Concorde.Handles.Faction.Faction_Handle;
         Company : Concorde.Handles.Company_Reference;
      end record;

   overriding function Identifier
     (Manager : Faction_Company_Manager)
      return String
   is ("faction company"
       & Concorde.Handles.To_String (Manager.Company) & " manager");

   overriding procedure On_Activation_Begin
     (Manager : in out Faction_Company_Manager);

   overriding procedure Create_Planning
     (Manager : in out Faction_Company_Manager)
   is null;

   overriding procedure Create_Bids
     (Manager : in out Faction_Company_Manager);

   overriding procedure Execute_Consumption
     (Manager : in out Faction_Company_Manager)
   is null;

   overriding procedure Execute_Production
     (Manager : in out Faction_Company_Manager)
   is null;

   -----------------
   -- Create_Bids --
   -----------------

   overriding procedure Create_Bids
     (Manager : in out Root_Faction_Manager)
   is

      procedure Check_Stock
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type);

      -----------------
      -- Check_Stock --
      -----------------

      procedure Check_Stock
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type)
      is
      begin
         if Concorde.Commodities.Is_Title (Commodity) then
            Manager.Create_Ask
              (Commodity => Commodity,
               Quantity  => Quantity,
               Price     => Concorde.Money.Price (Value, Quantity));
         elsif Concorde.Commodities.Is_Lease (Commodity) then
            Manager.Create_Ask
              (Commodity => Commodity,
               Quantity  => Quantity,
               Price     => Concorde.Money.Price (Value, Quantity));
         end if;
      end Check_Stock;

   begin
      Manager.Scan_Stock (Check_Stock'Access);
   end Create_Bids;

   -----------------
   -- Create_Bids --
   -----------------

   overriding procedure Create_Bids
     (Manager : in out Faction_Company_Manager)
   is

      procedure Check_Stock
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type);

      -----------------
      -- Check_Stock --
      -----------------

      procedure Check_Stock
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type)
      is
      begin
         if Concorde.Commodities.Is_Title (Commodity) then
            Manager.Create_Ask
              (Commodity => Commodity,
               Quantity  => Quantity,
               Price     => Concorde.Money.Price (Value, Quantity));
         elsif Concorde.Commodities.Is_Lease (Commodity) then
            Manager.Create_Ask
              (Commodity => Commodity,
               Quantity  => Quantity,
               Price     => Concorde.Money.Price (Value, Quantity));
         end if;
      end Check_Stock;

   begin
      Manager.Scan_Stock (Check_Stock'Access);
   end Create_Bids;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class) return Manager_Type
   is
      Manager : Root_Faction_Manager;
      Faction : constant Concorde.Handles.Faction.Faction_Type :=
        Concorde.Handles.Faction.Get_Faction (Managed);
   begin
      Manager.Faction := Faction.Get_Faction_Reference;
      Manager.Initialize_Agent_Manager
        (Agent          => Faction,
         Market         =>
           Concorde.Handles.Market.Get_By_World
             (Faction.Capital_World),
         Planning_Cycle => 10);
      Manager.Create_Bids;
      return new Root_Faction_Manager'(Manager);
   end Create_Default_Manager;

   ------------------------------------
   -- Create_Faction_Company_Manager --
   ------------------------------------

   function Create_Faction_Company_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Manager_Type
   is
      Manager : Faction_Company_Manager;
      Company : constant Concorde.Handles.Company.Company_Type :=
        Concorde.Handles.Company.Get_Company (Managed);
   begin
      Manager.Company := Company.Get_Company_Reference;
      Manager.Faction := Company.Faction;
      Manager.Initialize_Agent_Manager
        (Agent          => Company,
         Market         =>
           Concorde.Handles.Market.Get_By_World
             (Company.Headquarters),
         Planning_Cycle => 10);
      Manager.Create_Bids;
      return new Faction_Company_Manager'(Manager);
   end Create_Faction_Company_Manager;

   -------------------------
   -- On_Activation_Begin --
   -------------------------

   overriding procedure On_Activation_Begin
     (Manager : in out Faction_Company_Manager)
   is
   begin
      Concorde.Managers.Agents.Root_Agent_Manager_Type (Manager)
        .On_Activation_Begin;
      declare
         use Concorde.Money;
         Profit   : constant Money_Type :=
           Manager.Last_Earn - Manager.Last_Spend;
         Rate     : constant Unit_Real :=
           Concorde.Handles.Company.Get (Manager.Company).Dividend;
         Dividend : constant Money_Type :=
           (if Profit > Zero
            then Adjust (Profit, Rate)
            else Zero);
         Shares   : constant Natural :=
           Concorde.Handles.Company.Get (Manager.Company).Shares;
      begin
         if Dividend > Zero then
            Manager.Log ("dividend: " & Show (Dividend));

            for Shareholder of
              Concorde.Handles.Shareholder.Select_By_Company
                (Manager.Company)
            loop
               declare
                  Factor : constant Unit_Real :=
                    Real (Shareholder.Shares)
                    / Real (Shares);
                  Payment : constant Money_Type :=
                    Adjust (Dividend, Factor);
               begin
                  Manager.Log ("pay " & Show (Payment)
                               & " to Agent"
                               & Concorde.Handles.To_String (Shareholder.Agent));

                  Concorde.Agents.Add_Cash
                    (Concorde.Handles.Agent.Get
                       (Shareholder.Agent).Account,
                     Payment, "dividend");
               end;
            end loop;

            Manager.Spend (Dividend, "dividend");
         end if;
      end;
   end On_Activation_Begin;

end Concorde.Managers.Factions;
