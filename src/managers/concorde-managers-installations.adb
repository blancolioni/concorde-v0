with Concorde.Managers.Agents;

with Concorde.Commodities;

with Concorde.Money;
with Concorde.Quantities;

with Concorde.Handles.Facility;
with Concorde.Handles.Installation;
with Concorde.Handles.Market;

package body Concorde.Managers.Installations is

   type Root_Installation_Manager is
     abstract new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Installation : Concorde.Handles.Installation_Reference;
         Facility     : Concorde.Handles.Facility_Reference;
      end record;

   overriding function Identifier
     (Manager : Root_Installation_Manager)
      return String;

   procedure Initialize_Installation_Manager
     (Manager : in out Root_Installation_Manager'Class;
      Managed : Concorde.Handles.Managed.Managed_Class);

   type Default_Installation_Manager is
     new Root_Installation_Manager with
      record
         null;
      end record;

   overriding procedure Create_Planning
     (Manager : in out Default_Installation_Manager)
   is null;

   overriding procedure Create_Bids
     (Manager : in out Default_Installation_Manager)
   is null;

   overriding procedure Execute_Consumption
     (Manager : in out Default_Installation_Manager)
   is null;

   overriding procedure Execute_Production
     (Manager : in out Default_Installation_Manager)
   is null;

   type Agora_Installation_Manager is
     new Root_Installation_Manager with
      record
         null;
      end record;

   overriding procedure Create_Planning
     (Manager : in out Agora_Installation_Manager)
   is null;

   overriding procedure Create_Bids
     (Manager : in out Agora_Installation_Manager);

   overriding procedure Execute_Production
     (Manager : in out Agora_Installation_Manager)
   is null;

   overriding procedure Execute_Consumption
     (Manager : in out Agora_Installation_Manager)
   is null;

   -----------------
   -- Create_Bids --
   -----------------

   overriding procedure Create_Bids
     (Manager : in out Agora_Installation_Manager)
   is

      procedure Create_Ask
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type);

      ----------------
      -- Create_Ask --
      ----------------

      procedure Create_Ask
        (Commodity : Concorde.Handles.Commodity.Commodity_Class;
         Quantity  : Concorde.Quantities.Quantity_Type;
         Value     : Concorde.Money.Money_Type)
      is
         use Concorde.Quantities;
         Previous : constant Quantity_Type :=
           Manager.Previous_Ask (Commodity);
         Remaining : constant Quantity_Type :=
           Manager.Remaining_Ask (Commodity);
         Ask       : Quantity_Type;
         Min_Price : constant Concorde.Money.Price_Type :=
           Concorde.Money.Price (Value, Quantity);
      begin

         if Previous > Zero then
            if Remaining = Zero then
               Ask := Scale (Previous, 1.1);
            elsif Remaining < Previous then
               Ask := Previous - Remaining;
            else
               Ask := Previous;
            end if;
         else
            Ask := Scale (Quantity, 0.1);
         end if;

         Manager.Log
           (Concorde.Commodities.Local_Name (Commodity)
            & ": create ask: previous ask "
            & Concorde.Quantities.Show (Previous)
            & "; sold: "
            & Concorde.Quantities.Show (Previous - Remaining)
            & "; remaining "
            & Concorde.Quantities.Show (Remaining)
            & "; new ask "
            & Concorde.Quantities.Show (Min (Quantity, Ask)));

         Manager.Create_Ask
           (Commodity, Min (Quantity, Ask), Min_Price);

      end Create_Ask;

   begin
      Manager.Scan_Stock (Create_Ask'Access);
   end Create_Bids;

   ----------------------------------
   -- Create_Default_Agora_Manager --
   ----------------------------------

   function Create_Default_Agora_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class) return Manager_Type
   is
      Manager : Agora_Installation_Manager;
   begin
      Manager.Initialize_Installation_Manager (Managed);
      Manager.Create_Bids;
      return new Agora_Installation_Manager'(Manager);
   end Create_Default_Agora_Manager;

   ----------------------------
   -- Create_Default_Manager --
   ----------------------------

   function Create_Default_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Manager_Type
   is
      Manager : Default_Installation_Manager;
   begin
      Manager.Initialize_Installation_Manager (Managed);
      return new Default_Installation_Manager'(Manager);
   end Create_Default_Manager;

   ----------------
   -- Identifier --
   ----------------

   overriding function Identifier
     (Manager : Root_Installation_Manager)
      return String
   is
   begin
      return Concorde.Handles.Facility.Get (Manager.Facility).Tag
        & " "
        & Concorde.Handles.To_String (Manager.Installation)
        & " manager";
   end Identifier;

   -------------------------------------
   -- Initialize_Installation_Manager --
   -------------------------------------

   procedure Initialize_Installation_Manager
     (Manager : in out Root_Installation_Manager'Class;
      Managed : Concorde.Handles.Managed.Managed_Class)
   is
      Installation : constant Concorde.Handles.Installation.Installation_Type :=
        Concorde.Handles.Installation.Get_Installation
          (Managed);
   begin
      Manager.Installation := Installation.Get_Installation_Reference;
      Manager.Facility := Installation.Facility;
      Manager.Initialize_Agent_Manager
        (Agent  => Installation,
         Market =>
           Concorde.Handles.Market.Get_By_World
             (Installation.World),
         Planning_Cycle => 10);
   end Initialize_Installation_Manager;

end Concorde.Managers.Installations;
