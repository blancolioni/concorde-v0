with Concorde.Calendar;
with Concorde.Solar_System;
with Concorde.Star_Systems;
with Concorde.Worlds;

with Concorde.Db.World;

package body Concorde.UI.Models.Star_System is

   function "+" (Item : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Search_World
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real;
      Reference : out Concorde.Db.World_Reference;
      Index     : out Natural);

   ------------
   -- Create --
   ------------

   function Create
     (Star_System : Concorde.Db.Star_System_Reference)
      return Star_System_Model
   is
      Worlds : constant Concorde.Worlds.World_Selection :=
                 Concorde.Star_Systems.Worlds
                   (Star_System);
   begin
      return Model : constant Star_System_Model :=
        new Root_Star_System_Model
      do
         Model.Star_System := Star_System;
         Model.Add_Column ("name");
         Model.Add_Column ("earth-mass");
         Model.Add_Column ("earth-radius");
         Model.Add_Column ("g");
         Model.Add_Column ("temperature");
         Model.Add_Column ("habitability");
         Model.Add_Column ("water-coverage");
         Model.Add_Column ("ice-coverage");
         Model.Add_Column ("day-length");

         for World of Worlds.Get_Worlds loop

            declare
               use Concorde.Solar_System;
               Row   : constant Concorde.UI.Models.Tables.Table_Row_Index :=
                         Model.Add_Row;
               Rec   : constant Concorde.Db.World.World_Type :=
                         Concorde.Db.World.Get (World);
            begin
               Model.Set_Cell (Row, 1, Rec.Name);
               Model.Set_Cell (Row, 2, Rec.Mass / Earth_Mass);
               Model.Set_Cell (Row, 3, Rec.Radius / Earth_Radius);
               Model.Set_Cell (Row, 4, Rec.Surface_Gravity);
               Model.Set_Cell (Row, 5, Rec.Surface_Temperature - 273.0);
               Model.Set_Cell (Row, 6, Rec.Habitability);
               Model.Set_Cell (Row, 7, Rec.Water_Coverage);
               Model.Set_Cell (Row, 8, Rec.Ice_Coverage);
               Model.Set_Cell (Row, 9, Rec.Rotation_Period / 3600.0);

               Model.Vector.Append
                 (World_Record'
                    (Reference       => Rec.Get_World_Reference,
                     Name            => +Rec.Name,
                     Category        => Rec.Category,
                     Climate         => Rec.Climate,
                     Radius          => Rec.Radius / Earth_Radius,
                     Orbit           => Rec.Semimajor_Axis / Earth_Orbit,
                     Year            =>
                       Rec.Period / (Earth_Sidereal_Year * 86_400.0),
                     Start_Longitude => Rec.Zero_Longitude));
            end;
         end loop;
      end return;
   end Create;

   -----------------------
   -- Current_Longitude --
   -----------------------

   function Current_Longitude
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real
   is
      Standard_Years : constant Non_Negative_Real :=
                         Concorde.Calendar.To_Real
                           (Concorde.Calendar.Clock)
                           / 360.0;
      Local_Years    : constant Non_Negative_Real :=
                         Standard_Years / Model.Vector (Index).Year;
      Partial_Year   : constant Non_Negative_Real :=
                         Local_Years - Real'Truncation (Local_Years);
      Raw_Longitude  : constant Non_Negative_Real :=
                         360.0 * Partial_Year
                           + Model.Vector (Index).Start_Longitude;
      Longitude      : constant Non_Negative_Real :=
                         (if Raw_Longitude >= 360.0
                          then Raw_Longitude - 360.0
                          else Raw_Longitude);
   begin
      return Longitude;
   end Current_Longitude;

   ----------------
   -- Find_World --
   ----------------

   function Find_World
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real)
      return Concorde.Db.World_Reference
   is
      Reference : Concorde.Db.World_Reference;
      Index     : Natural;
   begin
      Model.Search_World (Orbit, Longitude, Reference, Index);
      return Reference;
   end Find_World;

   ----------------------
   -- Find_World_Index --
   ----------------------

   function Find_World_Index
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real)
      return Natural
   is
      Reference : Concorde.Db.World_Reference;
      Index     : Natural;
   begin
      Model.Search_World (Orbit, Longitude, Reference, Index);
      return Index;
   end Find_World_Index;

   ------------------
   -- Search_World --
   ------------------

   procedure Search_World
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real;
      Reference : out Concorde.Db.World_Reference;
      Index     : out Natural)
   is
      Long : constant Non_Negative_Real :=
               (if Longitude < 0.0
                then Longitude + 360.0
                elsif Longitude >= 360.0
                then Longitude - 360.0
                else Longitude);
   begin
      Reference := Concorde.Db.Null_World_Reference;
      Index     := 0;

      for I in 1 .. Model.Vector.Last_Index loop
         declare
            World_Orbit : constant Non_Negative_Real :=
                            Model.Orbit (I);
            World_Long  : constant Real := Model.Current_Longitude (I);
         begin
            if abs (Orbit - World_Orbit) / World_Orbit < 0.1
              and then abs (Long - World_Long) < 5.0
            then
               Reference := Model.Vector.Element (I).Reference;
               Index     := I;
               exit;
            end if;
         end;
      end loop;
   end Search_World;

end Concorde.UI.Models.Star_System;
