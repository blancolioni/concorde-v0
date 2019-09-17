with Concorde.Managers.Agents;

with Concorde.Commodities;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Db.Army;
with Concorde.Db.Market;
with Concorde.Db.Regiment;
with Concorde.Db.Supply_Input;
with Concorde.Db.Unit;

package body Concorde.Managers.Armies is

   type Root_Army_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Daily    : Concorde.Commodities.Stock_Type;
         Supplied : Concorde.Db.Supplied_Reference;
         Army     : Concorde.Db.Army_Reference;
      end record;

   overriding function Identifier
     (Manager : Root_Army_Manager)
      return String
   is ("army" & Concorde.Db.To_String (Manager.Army) & " manager");

   overriding procedure Create_Planning
     (Manager : in out Root_Army_Manager);

   overriding procedure Create_Bids
     (Manager : in out Root_Army_Manager);

   overriding procedure Execute_Consumption
     (Manager : in out Root_Army_Manager);

   overriding procedure Execute_Production
     (Manager : in out Root_Army_Manager)
   is null;

   overriding procedure Create_Bids
     (Manager : in out Root_Army_Manager)
   is

      procedure Bid
        (Commodity : Concorde.Commodities.Commodity_Reference;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type);

      ---------
      -- Bid --
      ---------

      procedure Bid
        (Commodity : Concorde.Commodities.Commodity_Reference;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type)
      is
         pragma Unreferenced (Value);
         use Concorde.Quantities;
         Want : constant Quantity_Type := Scale (Quantity, 10.0);
         Have : constant Quantity_Type := Manager.Stock_Quantity (Commodity);
         Max  : constant Quantity_Type := Scale (Quantity, 1.1);
         Bid_Quantity : constant Quantity_Type :=
           Min (Max, Want - Have);
      begin
         if Have < Want then
            Manager.Create_Bid
              (Commodity => Commodity,
               Quantity  => Bid_Quantity,
               Price     =>
                 Manager.Current_Ask_Price (Commodity, Bid_Quantity));
         end if;
      end Bid;

   begin
      Manager.Daily.Iterate (Bid'Access);
   end Create_Bids;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference) return Manager_Type
   is
      Manager : Root_Army_Manager;
      Army : constant Concorde.Db.Army.Army_Type :=
        Concorde.Db.Army.Get_Army (Managed);
   begin
      Manager.Army := Army.Get_Army_Reference;
      Manager.Supplied := Army.Get_Supplied_Reference;
      Manager.Initialize_Agent_Manager
        (Agent          => Army,
         Market         =>
           Concorde.Db.Market.Get_Reference_By_World
             (Army.World),
         Planning_Cycle => 10);
      return new Root_Army_Manager'(Manager);
   end Create_Default_Manager;

   ---------------------
   -- Create_Planning --
   ---------------------

   overriding procedure Create_Planning
     (Manager : in out Root_Army_Manager)
   is
      Daily : Concorde.Commodities.Stock_Type renames Manager.Daily;
   begin
      Daily.Clear;

      for Regiment of
        Concorde.Db.Regiment.Select_By_Army (Manager.Army)
      loop
         declare
            Unit : constant Concorde.Db.Unit.Unit_Type :=
              Concorde.Db.Unit.Get (Regiment.Unit);
         begin
            for Supply_Input of
              Concorde.Db.Supply_Input.Select_By_Supplied
                (Unit.Get_Supplied_Reference)
            loop
               declare
                  Commodity : constant Commodities.Commodity_Reference :=
                    Commodities.Get_Commodity (Supply_Input.Commodity);
               begin
                  Daily.Add_Quantity
                    (Commodity,
                     Supply_Input.Quantity,
                     Manager.Current_Ask_Price
                       (Commodity, Supply_Input.Quantity));
               end;
            end loop;
         end;
      end loop;

      Manager.Log
        ("daily budget: "
         & Concorde.Money.Show (Daily.Total_Value));

   end Create_Planning;

   overriding procedure Execute_Consumption
     (Manager : in out Root_Army_Manager)
   is
      Daily : Concorde.Commodities.Stock_Type;
   begin
      for Regiment of
        Concorde.Db.Regiment.Select_By_Army (Manager.Army)
      loop
         declare
            Unit : constant Concorde.Db.Unit.Unit_Type :=
              Concorde.Db.Unit.Get (Regiment.Unit);
         begin
            for Supply_Input of
              Concorde.Db.Supply_Input.Select_By_Supplied
                (Unit.Get_Supplied_Reference)
            loop
               declare
                  Commodity : constant Commodities.Commodity_Reference :=
                    Commodities.Get_Commodity (Supply_Input.Commodity);
               begin
                  Daily.Add_Quantity
                    (Commodity,
                     Supply_Input.Quantity,
                     Manager.Current_Ask_Price
                       (Commodity, Supply_Input.Quantity));
               end;
            end loop;
         end;
      end loop;

      declare

         Organisation : Unit_Real := 1.0;

         procedure Consume
           (Commodity : Concorde.Commodities.Commodity_Reference;
            Quantity  : Concorde.Quantities.Quantity_Type;
            Value     : Concorde.Money.Money_Type);

         -------------
         -- Consume --
         -------------

         procedure Consume
           (Commodity : Concorde.Commodities.Commodity_Reference;
            Quantity  : Concorde.Quantities.Quantity_Type;
            Value     : Concorde.Money.Money_Type)
         is
            pragma Unreferenced (Value);
            use Concorde.Quantities;
            Have : constant Quantity_Type :=
              Manager.Stock_Quantity (Commodity);
            Consumed : constant Quantity_Type := Min (Have, Quantity);
         begin
            if Consumed < Quantity then
               Organisation :=
                 Unit_Real'Min (Organisation,
                                To_Real (Consumed)
                                / To_Real (Quantity));
            end if;
            Manager.Remove_Stock (Commodity, Quantity);
         end Consume;

      begin
         Daily.Iterate (Consume'Access);

         for Regiment of
           Concorde.Db.Regiment.Select_By_Army (Manager.Army)
         loop
            Concorde.Db.Regiment.Update_Regiment
              (Regiment.Get_Regiment_Reference)
              .Set_Morale
                (Unit_Clamp
                   (Regiment.Morale
                    + (Organisation - Regiment.Morale) / 10.0))
                .Done;
         end loop;
      end;

   end Execute_Consumption;

end Concorde.Managers.Armies;
