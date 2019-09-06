with Concorde.Db.Pop;
with Concorde.Db.Pop_Group;

with Concorde.Db.Consumer_Good;
with Concorde.Db.Pop_Group_Needs;

package body Concorde.Managers.Pops is

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Db.Managed_Reference)
      return Manager_Type
   is
      use Concorde.Db;
      Pop     : constant Concorde.Db.Pop.Pop_Type :=
                  Concorde.Db.Pop.Get_Pop
                    (Managed);
      Group   : constant Concorde.Db.Pop_Group.Pop_Group_Type :=
                  Concorde.Db.Pop_Group.Get (Pop.Pop_Group);
      Manager : Root_Pop_Manager :=
                  (Concorde.Managers.Agents.Root_Agent_Manager with
                   Pop                 => Pop.Get_Pop_Reference,
                   Group               => Pop.Pop_Group,
                   Commodity           => Group.Get_Commodity_Reference,
                   Employer            => Pop.Employer,
                   Employed            =>
                     Pop.Employer /= Null_Employer_Reference,
                   Consumption_Quality => Group.Consumer_Quality,
                   Service_Quality     => Group.Service_Quality);
   begin
      Manager.Initialize_Agent_Manager (Pop, Pop.World);
      return new Root_Pop_Manager'(Manager);
   end Create_Default_Manager;

   --------------------------
   -- Create_Market_Offers --
   --------------------------

   overriding procedure Create_Market_Offers
     (Manager : in out Root_Pop_Manager)
   is
   begin
      Concorde.Managers.Agents.Root_Agent_Manager (Manager)
        .Create_Market_Offers;
      if not Manager.Employed then
         Manager.Place_Ask
           (Commodity => Manager.Commodity,
            Quantity  =>
              Concorde.Db.Pop.Get (Manager.Pop).Size,
            Price     => Manager.Current_Market_Bid_Price (Manager.Commodity));
      end if;

   end Create_Market_Offers;

   -------------------------
   -- Execute_Agent_Tasks --
   -------------------------

   overriding procedure Execute_Agent_Tasks
     (Manager : in out Root_Pop_Manager)
   is
      Consumer_Happiness : Unit_Real := 1.0;

      function Use_Item
        (Commodity : Concorde.Db.Commodity_Reference)
         return Unit_Real;

      --------------
      -- Use_Item --
      --------------

      function Use_Item
        (Commodity : Concorde.Db.Commodity_Reference)
         return Unit_Real
      is
         use Concorde.Quantities;
         Available : constant Quantity_Type :=
                       Manager.Current_Stock (Commodity);
         Required  : constant Quantity_Type :=
                       Manager.Size;
         Consumed  : constant Quantity_Type :=
                       Min (Available, Required);
      begin
         Manager.Remove_Stock (Commodity, Consumed);

         if Consumed < Required then
            return To_Real (Consumed) / To_Real (Required);
         else
            return 1.0;
         end if;
      end Use_Item;

   begin
      for Item of
        Concorde.Db.Consumer_Good.Select_By_Quality
          (Manager.Consumption_Quality)
      loop
         Consumer_Happiness :=
           Unit_Real'Min (Consumer_Happiness,
                          Use_Item (Item.Get_Commodity_Reference));
      end loop;

      declare
         Pop : constant Concorde.Db.Pop.Pop_Type :=
                 Concorde.Db.Pop.Get (Manager.Pop);
      begin
         if Pop.Happiness < Consumer_Happiness then
            Pop.Set_Happiness (Pop.Happiness
                               + (Consumer_Happiness - Pop.Happiness)
                               / 10.0);
         else
            Pop.Set_Happiness (Pop.Happiness
                               + (Consumer_Happiness - Pop.Happiness)
                               / 5.0);
         end if;
      end;
   end Execute_Agent_Tasks;

   ------------------------
   -- Get_Required_Stock --
   ------------------------

   overriding procedure Get_Required_Stock
     (Manager : Root_Pop_Manager;
      Stock   : in out Concorde.Commodities.Stock_Type)
   is
   begin
      for Item of
        Concorde.Db.Pop_Group_Needs.Select_By_Pop_Group
          (Manager.Group)
      loop
         Stock.Set_Quantity
           (Item.Commodity, Manager.Size,
            Manager.Current_Market_Ask_Price (Item.Commodity));
      end loop;
   end Get_Required_Stock;

   ----------
   -- Size --
   ----------

   function Size
     (Manager : Root_Pop_Manager'Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Db.Pop.Get (Manager.Pop).Size;
   end Size;

end Concorde.Managers.Pops;
