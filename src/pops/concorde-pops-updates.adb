with Concorde.Markets;
with Concorde.Stock;

with Concorde.Updates.Events;

with Concorde.Handles.World;

with Concorde.Db;

package body Concorde.Pops.Updates is

   type Pop_Update is
     new Concorde.Updates.Root_Update_Type with
      record
         Pop    : Concorde.Handles.Pop.Pop_Handle;
         Group  : Concorde.Handles.Pop_Group.Pop_Group_Handle;
         World  : Concorde.Handles.World.World_Handle;
         Colony : Concorde.Handles.Colony.Colony_Handle;
         Market : Concorde.Markets.Concorde_Market;
      end record;

   overriding function Name
     (Update : Pop_Update)
      return String
   is (Describe (Update.Pop) & " update");

   overriding procedure Execute
     (Update : Pop_Update);

   -----------------------
   -- Create_Pop_Update --
   -----------------------

   procedure Create_Pop_Update (Pop : Concorde.Handles.Pop.Pop_Class) is
   begin
      Concorde.Updates.Events.Update_At
        (Clock  => Pop.Next_Event,
         Update => Pop_Update'
           (Concorde.Updates.Root_Update_Type with
            Pop => Pop.To_Pop_Handle,
            Group  => Pop.Pop_Group.To_Pop_Group_Handle,
            World  => Pop.World.To_World_Handle,
            Colony => Pop.Colony.To_Colony_Handle,
            Market => Concorde.Markets.World_Market (Pop.World)));
   end Create_Pop_Update;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Pop_Update)
   is
      use Concorde.Quantities;
      Employable : constant Concorde.Quantities.Quantity_Type :=
                     Concorde.Stock.Get_Quantity
                       (Has_Stock => Update.Pop,
                        Commodity => Update.Group);
   begin

      Log (Update.Pop,
           "executing update; employable = "
           & Show (Employable));

      if Employable > Zero then

         Concorde.Markets.Create_Offer
           (Market    => Update.Market,
            Agent     => Update.Pop,
            Offer     => Concorde.Db.Ask,
            Commodity => Update.Group,
            Quantity  => Employable,
            Price     =>
              Concorde.Markets.Historical_Mean_Price
                (Update.Market, Update.Group));
      end if;
   end Execute;

end Concorde.Pops.Updates;
