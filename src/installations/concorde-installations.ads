with Concorde.Quantities;

with Concorde.Handles.Commodity;
with Concorde.Handles.Installation;

package Concorde.Installations is

   subtype Installation_Handle is
     Concorde.Handles.Installation.Installation_Handle;

   subtype Installation_Class is
     Concorde.Handles.Installation.Installation_Class;

   function Describe
     (Installation : Installation_Class)
      return String;

   procedure Log
     (Installation     : Installation_Class;
      Message          : String);

   procedure Set_Production
     (Installation : Installation_Class;
      Production   : Concorde.Handles.Commodity.Commodity_Class);

   function Empty_Queue
     (Installation : Installation_Class)
      return Boolean;

   function First_Queued_Commodity
     (Installation : Installation_Class)
      return Concorde.Handles.Commodity.Commodity_Class
     with Pre => not Empty_Queue (Installation);

   function First_Queued_Quantity
     (Installation : Installation_Class)
      return Concorde.Quantities.Quantity_Type
     with Pre => not Empty_Queue (Installation);

   procedure Update_Queue_First
     (Installation : Installation_Class;
      Quantity     : Concorde.Quantities.Quantity_Type)
     with Pre => not Empty_Queue (Installation);

   procedure Queue_Production
     (Installation : Installation_Class;
      Commodity    : Concorde.Handles.Commodity.Commodity_Class;
      Quantity     : Concorde.Quantities.Quantity_Type)
     with Post => not Empty_Queue (Installation);

   procedure Queue_Capacity_Production
     (Installation : Installation_Class;
      Commodity    : Concorde.Handles.Commodity.Commodity_Class)
     with Post => not Empty_Queue (Installation);

   procedure Iterate_Queue
     (Installation : Installation_Class;
      Process      : not null access
        procedure (Commodity    : Concorde.Handles.Commodity.Commodity_Class;
                   Quantity     : Concorde.Quantities.Quantity_Type));

end Concorde.Installations;
