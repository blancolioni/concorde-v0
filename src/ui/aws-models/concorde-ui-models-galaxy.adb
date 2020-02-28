with Concorde.UI.Models.Data_Source;
with Concorde.UI.Models.Values;

with Concorde.Color;
with Concorde.Solar_System;

with Concorde.Factions;
with Concorde.Star_Systems;

with Concorde.Db.Star;
with Concorde.Db.Star_System;
with Concorde.Db.Star_System_Distance;

package body Concorde.UI.Models.Galaxy is

   type Galaxy_Model_Type is
     new Concorde.UI.Models.Data_Source.Simple_Data_Source_Model with
      record
         null;
      end record;

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      User      : Concorde.Db.User_Reference;
      Arguments : String);

   overriding function Name
     (Model : Galaxy_Model_Type)
      return String
   is ("galaxy-data-source");

   overriding function Default_View_Name
     (Model : Galaxy_Model_Type)
      return String
   is ("Galaxy");

   ------------------------
   -- Galaxy_Model --
   ------------------------

   function Galaxy_Model
      return Root_Concorde_Model'Class
   is
   begin
      return Model : Galaxy_Model_Type;
   end Galaxy_Model;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      User      : Concorde.Db.User_Reference;
      Arguments : String)
   is
      pragma Unreferenced (Arguments);
      use Concorde.Db;

      Faction : constant Concorde.Factions.Faction_Type'Class :=
        Concorde.Factions.Get_User_Faction (User);
      Capital : constant Concorde.Db.Star_System_Reference :=
        (if Faction.Has_Element
         then Faction.Capital_System
         else Concorde.Db.Null_Star_System_Reference);
      Position : constant Concorde.Star_Systems.Interstellar_Position :=
        (if Capital /= Concorde.Db.Null_Star_System_Reference
         then Concorde.Star_Systems.Position (Capital)
         else (0.0, 0.0, 0.0));
   begin
      Model.Add_Column
        (Id       => "name",
         Label    => "Name",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "x",
         Label    => "X",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "y",
         Label    => "Y",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "z",
         Label    => "Z",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "mass",
         Label    => "Mass",
         Col_Type => Values.Real_Type);

      Model.Add_Column
        (Id       => "color",
         Label    => "Color",
         Col_Type => Values.Text_Type);

      declare

         procedure Add_Row
           (Star_System : Concorde.Db.Star_System.Star_System_Type);

         -------------
         -- Add_Row --
         -------------

         procedure Add_Row
           (Star_System : Concorde.Db.Star_System.Star_System_Type)
         is
            function T (S : String) return Values.Model_Value_Type
                        renames Values.Text_Value;
            function R (X : Real) return Values.Model_Value_Type
                        renames Values.Real_Value;

            Star : constant Concorde.Db.Star.Star_Type :=
              Concorde.Db.Star.First_By_Star_System
                (Star_System.Get_Star_System_Reference);
            Color : constant String :=
                      Concorde.Color.To_Html_String
                        (Star.Red, Star.Green, Star.Blue);
         begin
            Model.Add_Row
              ((T (Star_System.Name),
               R (Star_System.X - Position.X),
               R (Star_System.Y - Position.Y),
               R (Star_System.Z - Position.Z),
               R (Star.Mass / Concorde.Solar_System.Solar_Mass),
               T (Color)));
         end Add_Row;

      begin
         if Faction.Has_Element then
            Add_Row (Concorde.Db.Star_System.Get (Capital));
            for Distance of
              Concorde.Db.Star_System_Distance.Select_By_From
                (Capital)
            loop
               Add_Row
                 (Concorde.Db.Star_System.Get (Distance.To));
            end loop;
         else
            for Star_System of Concorde.Db.Star_System.Scan_By_Name loop
               Add_Row (Star_System);
            end loop;
         end if;
      end;

   end Start;

end Concorde.UI.Models.Galaxy;
