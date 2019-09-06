with WL.Localisation;

with Concorde.Solar_System;

with Concorde.Db.Faction;
with Concorde.Db.Star;
with Concorde.Db.Star_System;
with Concorde.Db.Star_System_Distance;

package body Concorde.UI.Models.Galaxy is

   -------------------------
   -- Create_Galaxy_Model --
   -------------------------

   function Create_Galaxy_Model
      return Galaxy_Model
   is
   begin
      return Model : constant Galaxy_Model :=
        new Root_Galaxy_Model
      do

         Model.Add_Column ("name");
         Model.Add_Column ("spectral-class");
         Model.Add_Column ("solar-masses");
         Model.Add_Column ("solar-radii");
         Model.Add_Column ("solar-luminosity");

         for Star_System of Concorde.Db.Star_System.Scan_By_Name loop
            declare
               use Ada.Strings.Unbounded;
               use Concorde.Solar_System;
               Star : constant Concorde.Db.Star.Star_Type :=
                        Concorde.Db.Star.First_By_Star_System
                          (Star_System.Get_Star_System_Reference);
               Color : constant Concorde.Color.Concorde_Color :=
                         (Star.Red, Star.Green, Star.Blue, 1.0);
               Row   : constant Concorde.UI.Models.Tables.Table_Row_Index :=
                         Model.Add_Row;
               Nearest : Near_System_Lists.List;
            begin
               for Near of
                 Concorde.Db.Star_System_Distance
                   .Select_Star_System_Range_Bounded_By_Distance
                     (From            => Star_System.Get_Star_System_Reference,
                      Start_Distance  => 0.0,
                      Finish_Distance => 7.0)
               loop
                  Nearest.Append ((Near.To, Near.Distance));
               end loop;

               Model.Vector.Append
                 (Star_Record'
                    (Reference    => Star_System.Get_Star_System_Reference,
                     Star_System  =>
                       Concorde.Star_Systems.Get
                         (Star_System.Get_Star_System_Reference),
                     Name         => To_Unbounded_String (Star_System.Name),
                     X            => Star_System.X,
                     Y            => Star_System.Y,
                     Z            => Star_System.Z,
                     Color        => Color,
                     Mass         => Star.Mass,
                     Radius       => Star.Radius,
                     Luminosity   => Star.Luminosity,
                     Near_Systems => Nearest));
               Model.Set_Cell (Row, 1, Star_System.Name);
               Model.Set_Cell (Row, 2, "G2");
               Model.Set_Cell (Row, 3, Star.Mass / Solar_Mass);
               Model.Set_Cell (Row, 4, Star.Radius / Solar_Radius);
               Model.Set_Cell (Row, 5, Star.Luminosity);

            end;
         end loop;
      end return;
   end Create_Galaxy_Model;

   ---------------
   -- Find_Star --
   ---------------

   function Find_Star
     (Model   : Root_Galaxy_Model'Class;
      X, Y, Z : Real)
      return Concorde.Db.Star_System_Reference
   is
      pragma Unreferenced (Z);
      Shortest : Non_Negative_Real := Non_Negative_Real'Last;
      Closest  : Concorde.Db.Star_System_Reference :=
                   Concorde.Db.Null_Star_System_Reference;
   begin
      for Star_Rec of Model.Vector loop
         declare
            D : constant Non_Negative_Real :=
                  (Star_Rec.X - X) ** 2
                  + (Star_Rec.Y - Y) ** 2;
         begin
            if D < Shortest then
               Shortest := D;
               Closest := Star_Rec.Reference;
            end if;
         end;
      end loop;

      if Shortest < 0.1 then
         return Closest;
      else
         return Concorde.Db.Null_Star_System_Reference;
      end if;
   end Find_Star;

   -----------------------
   -- Scan_Near_Systems --
   -----------------------

   procedure Scan_Near_Systems
     (Model       : Root_Galaxy_Model'Class;
      Star_System : Concorde.Star_Systems.Star_System_Type'Class;
      Process     : not null access
        procedure (Near_System : Concorde.Star_Systems.Star_System_Type'Class;
                   Distance    : Non_Negative_Real))
   is
      use Concorde.Db;
   begin
      for Star_Rec of Model.Vector loop
         if Star_Rec.Reference = Star_System.Reference then
            for Near_System of Star_Rec.Near_Systems loop
               Process (Concorde.Star_Systems.Get (Near_System.Star_System),
                        Near_System.Distance);
            end loop;
            return;
         end if;
      end loop;
   end Scan_Near_Systems;

   ----------------
   -- Scan_Stars --
   ----------------

   procedure Scan_Star_Systems
     (Model   : Root_Galaxy_Model'Class;
      Process : not null access
        procedure (Star_System : Concorde.Star_Systems.Star_System_Type'Class))
   is
   begin
      for Star_Rec of Model.Vector loop
         Process (Star_Rec.Star_System);
      end loop;
   end Scan_Star_Systems;

   ----------------
   -- Star_Owner --
   ----------------

   function Star_Owner
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Concorde.Db.Faction_Reference
   is
   begin
      return Concorde.Db.Faction.First_Reference_By_Capital_System
        (Model.Vector.Element (Index).Reference);
   end Star_Owner;

   -----------
   -- Title --
   -----------

   overriding function Title
     (Model : Root_Galaxy_Model)
      return String
   is
      pragma Unreferenced (Model);
   begin
      return WL.Localisation.Local_Text ("galaxy-map");
   end Title;

end Concorde.UI.Models.Galaxy;
