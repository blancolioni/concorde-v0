with Concorde.Commodities;
with Concorde.Quantities;
with Concorde.Money;

with Concorde.Managers.Agents;

with Concorde.Db.Faction;
with Concorde.Db.Market;

package body Concorde.Managers.Factions is

   type Root_Faction_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Faction : Concorde.Db.Faction_Reference;
      end record;

   overriding function Identifier
     (Manager : Root_Faction_Manager)
      return String
   is ("faction" & Concorde.Db.To_String (Manager.Faction) & " manager");

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

   -----------------
   -- Create_Bids --
   -----------------

   overriding procedure Create_Bids
     (Manager : in out Root_Faction_Manager)
   is

      procedure Check_Stock
        (Commodity : Concorde.Commodities.Commodity_Reference;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type);

      -----------------
      -- Check_Stock --
      -----------------

      procedure Check_Stock
        (Commodity : Concorde.Commodities.Commodity_Reference;
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
     (Managed : Concorde.Db.Managed_Reference) return Manager_Type
   is
      Manager : Root_Faction_Manager;
      Faction : constant Concorde.Db.Faction.Faction_Type :=
        Concorde.Db.Faction.Get_Faction (Managed);
   begin
      Manager.Faction := Faction.Get_Faction_Reference;
      Manager.Initialize_Agent_Manager
        (Agent          => Faction,
         Market         =>
           Concorde.Db.Market.Get_Reference_By_World
             (Faction.Capital_World),
         Planning_Cycle => 10);
      Manager.Create_Bids;
      return new Root_Faction_Manager'(Manager);
   end Create_Default_Manager;

end Concorde.Managers.Factions;
