with Concorde.Agents;
with Concorde.Markets;
with Concorde.Stock;
with Concorde.Real_Images;

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

   overriding procedure Execute_Consumption
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

   -------------------------
   -- Execute_Consumption --
   -------------------------

   overriding procedure Execute_Consumption
     (Manager : in out Pop_Manager)
   is
      use Concorde.Money, Concorde.Quantities;
      Happiness : Unit_Real := 0.0;
      Total     : Money_Type := Zero;
   begin
      for Consumer of
        Concorde.Handles.Consumer_Commodity.Select_By_Quality
          (Manager.Group.Consumer_Demand)
      loop
         declare
            Required : constant Quantity_Type :=
                         To_Quantity (Manager.Pop.Size
                                      * Consumer.Consumption);
            Available : constant Quantity_Type :=
                          Concorde.Stock.Get_Quantity
                            (Manager.Pop, Consumer);
            Consumed  : constant Quantity_Type :=
                          Min (Required, Available);
            Happy     : constant Unit_Real :=
                          To_Real (Consumed) / To_Real (Required)
                        * Consumer.Happiness;
            Value     : Money_Type := Zero;
         begin
            if Consumed > Zero then
               Concorde.Stock.Remove_Stock
                 (Manager.Pop, Consumer, Consumed, Value);
               Total := Total + Value;
            end if;

            Manager.Log
              (Consumer.Tag
               & ": required " & Show (Required)
               & "; available " & Show (Available)
               & "; consumed " & Show (Consumed)
               & "; cost " & Show (Value)
               & "; happiness "
               & Concorde.Real_Images.Approximate_Image (Happy * 100.0)
               & "%");

            Happiness := Happiness + Happy;
         end;
      end loop;

      declare
         New_Happiness : constant Unit_Real :=
                           Manager.Pop.Happiness * 0.5
                             + Happiness * 0.5;
      begin
         Manager.Log ("total cost " & Show (Total)
                      & "; new happiness "
                      & Concorde.Real_Images.Approximate_Image
                        (New_Happiness * 100.0)
                      & "%");
         Manager.Pop.Update_Pop
           .Set_Happiness (New_Happiness)
           .Done;
      end;

   end Execute_Consumption;

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
         declare
            use Concorde.Quantities;
            Have : constant Quantity_Type :=
                     Concorde.Stock.Get_Quantity
                       (Manager.Pop, Consumer);
            Necessary : constant Quantity_Type :=
                          Concorde.Quantities.To_Quantity
                            (Manager.Pop.Size
                             * Consumer.Consumption);
            Desired   : constant Quantity_Type :=
                          Concorde.Quantities.To_Quantity
                            (Manager.Pop.Size / Consumer.Happiness
                             * Consumer.Consumption);
         begin
            if Necessary > Have or else Desired > Have then
               Manager.Add_Requirement
                 (Commodity => Consumer,
                  Necessary =>
                    (if Necessary > Have then Necessary - Have else Zero),
                  Desired   =>
                    (if Desired > Have then Desired - Have else Zero));
            end if;
         end;
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
