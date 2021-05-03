with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Identifiers;
with Concorde.Stock;

with Concorde.Logging;
with Concorde.Real_Images;

with Concorde.Handles.Commodity;
with Concorde.Handles.Consumer_Commodity;
with Concorde.Handles.Employment;

package body Concorde.Pops is

   --------------
   -- Describe --
   --------------

   function Describe (Pop : Concorde.Handles.Pop.Pop_Class) return String is
   begin
      return Concorde.Real_Images.Approximate_Image (Pop.Size)
        & " " & Pop.Pop_Group.Tag;
   end Describe;

   ---------
   -- Log --
   ---------

   procedure Log (Pop     : Concorde.Handles.Pop.Pop_Class;
                  Message : String)
   is
   begin
      Concorde.Logging.Log
        (Describe (Pop), Message);
   end Log;

   -------------
   -- New_Pop --
   -------------

   procedure New_Pop
     (Faction : Concorde.Handles.Faction.Faction_Class;
      Colony  : Concorde.Handles.Colony.Colony_Class;
      Sector  : Concorde.Handles.World_Sector.World_Sector_Class;
      Group   : Concorde.Handles.Pop_Group.Pop_Group_Class;
      Size    : Concorde.Quantities.Quantity_Type;
      Cash    : Concorde.Money.Money_Type)
   is
      Pop : constant Concorde.Handles.Pop.Pop_Handle :=
              Concorde.Handles.Pop.Create
                (Account      =>
                           Concorde.Agents.New_Account (Cash),
                 Last_Earn    => Concorde.Money.Zero,
                 Last_Spend   => Concorde.Money.Zero,
                 Active       => True,
                 Scheduled    => False,
                 Next_Event   => Concorde.Calendar.Clock,
                 Manager      => "default-pop",
                 Identifier   => Concorde.Identifiers.Next_Identifier,
                 Faction      => Faction,
                 Colony       => Colony,
                 World        => Sector.World,
                 World_Sector => Sector,
                 Pop_Group    => Group,
                 Size         => Concorde.Quantities.To_Real (Size),
                 Apathy       => 0.0,
                 Loyalty      => 1.0,
                 Happiness    => 1.0);
   begin
      Concorde.Stock.Add_Initial_Stock
        (To       => Pop,
         Item     => Group,
         Quantity => Size);

      for Consumer of
        Concorde.Handles.Consumer_Commodity.Select_By_Quality
          (Group.Consumer_Demand)
      loop
         declare
            use Concorde.Quantities;
            Necessary : constant Quantity_Type :=
                          Concorde.Quantities.Scale
                            (Size, Consumer.Consumption);
         begin
            Concorde.Stock.Add_Initial_Stock
              (Pop, Concorde.Handles.Commodity.Get_By_Tag (Consumer.Tag),
               Necessary);
         end;
      end loop;

   end New_Pop;

   -------------------
   -- On_Employment --
   -------------------

   procedure On_Employment
     (Pop      : Concorde.Handles.Pop.Pop_Class;
      Employer : Concorde.Handles.Employer.Employer_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Salary   : Concorde.Money.Price_Type)
   is
      use type Concorde.Quantities.Quantity_Type;
      Handle : constant Concorde.Handles.Employment.Employment_Class :=
                 Concorde.Handles.Employment.Get_By_Employment
                   (Pop, Employer);
   begin
      if Handle.Has_Element then
         Handle.Update_Employment
           .Set_Quantity (Handle.Quantity + Quantity)
           .Set_Salary (Salary)
           .Done;
      else
         Concorde.Handles.Employment.Create
           (Pop      => Pop,
            Employer => Employer,
            Quantity => Quantity,
            Salary   => Salary);
      end if;
   end On_Employment;

end Concorde.Pops;
