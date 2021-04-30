with Concorde.Agents;
with Concorde.Markets;
with Concorde.Stock;

with Concorde.Managers.Agents;

with Concorde.Handles.Consumer_Commodity;
with Concorde.Handles.World;

package body Concorde.Pops.Managers is

   type Pop_Manager is
     new Concorde.Managers.Agents.Root_Agent_Manager_Type with
      record
         Pop    : Concorde.Handles.Pop.Pop_Handle;
         Group  : Concorde.Handles.Pop_Group.Pop_Group_Handle;
         World  : Concorde.Handles.World.World_Handle;
         Colony : Concorde.Handles.Colony.Colony_Handle;
      end record;

   type Pop_Manager_Access is access all Pop_Manager'Class;

   overriding function Identifier
     (Manager : Pop_Manager)
      return String
   is (Describe (Manager.Pop) & " update");

   overriding procedure Set_Requirements
     (Manager : in out Pop_Manager);

   overriding procedure Set_Sale_Stock
     (Manager : in out Pop_Manager);

   -----------------------
   -- Create_Pop_Update --
   -----------------------

   function Create_Pop_Manager
     (Managed : Concorde.Handles.Managed.Managed_Class)
      return Concorde.Managers.Manager_Type
   is
      use Concorde.Handles.Pop;
      Pop : constant Pop_Handle :=
              Get_From_Managed (Managed);
      Manager      : constant Pop_Manager_Access :=
                       new Pop_Manager'
                         (Concorde.Managers.Agents.Root_Agent_Manager_Type with
                          Pop => Pop,
                          Group  => Pop.Pop_Group.To_Pop_Group_Handle,
                          World  => Pop.World.To_World_Handle,
                          Colony => Pop.Colony.To_Colony_Handle);
   begin
      Concorde.Agents.Log_Agent
        (Pop,
         Describe (Pop),
         "using pop manager");
      Manager.Initialize_Agent_Manager
        (Agent          => Pop,
         Market         =>
           Concorde.Markets.World_Market (Pop.World),
         Planning_Cycle => 10);
      return Concorde.Managers.Manager_Type (Manager);
   end Create_Pop_Manager;

   ----------------------
   -- Set_Requirements --
   ----------------------

   overriding procedure Set_Requirements
     (Manager : in out Pop_Manager)
   is
   begin
      for Consumer of
        Concorde.Handles.Consumer_Commodity.Select_By_Quality
          (Manager.Group.Consumer_Demand)
      loop
         Manager.Add_Requirement
           (Commodity => Consumer,
            Necessary =>
              Concorde.Quantities.To_Quantity
                (Manager.Pop.Size),
            Desired   =>
              Concorde.Quantities.To_Quantity
                (Manager.Pop.Size / Consumer.Happiness));
      end loop;
   end Set_Requirements;

   --------------------
   -- Set_Sale_Stock --
   --------------------

   overriding procedure Set_Sale_Stock
     (Manager : in out Pop_Manager)
   is
      use Concorde.Quantities;
      Employable : constant Concorde.Quantities.Quantity_Type :=
                     Concorde.Stock.Get_Quantity
                       (Has_Stock => Manager.Pop,
                        Commodity => Manager.Group);
   begin
      if Employable > Zero then
         Manager.Add_Ask
           (Manager.Group, Employable,
            Concorde.Money.Total
              (Manager.Historical_Mean_Price (Manager.Group),
               Employable));
      end if;
   end Set_Sale_Stock;

end Concorde.Pops.Managers;
