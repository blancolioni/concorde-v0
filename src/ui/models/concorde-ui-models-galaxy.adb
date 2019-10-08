with Concorde.UI.Models.Data_Source;
with Concorde.UI.Models.Values;

with Concorde.Db.Star_System;

package body Concorde.UI.Models.Galaxy is

   type Galaxy_Model_Type is
     new Concorde.UI.Models.Data_Source.Simple_Data_Source_Model with
      record
         null;
      end record;

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      Arguments : String);

   overriding function Name
     (Model : Galaxy_Model_Type)
      return String
   is ("galaxy-data-source");

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
      Arguments : String)
   is
      pragma Unreferenced (Arguments);
   begin
      Model.Add_Column
        (Id       => "name",
         Label    => "Name",
         Col_Type => Values.Text_Type);
      Model.Add_Column
        (Id       => "system-x",
         Label    => "X",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "system-y",
         Label    => "Y",
         Col_Type => Values.Real_Type);
      Model.Add_Column
        (Id       => "system-z",
         Label    => "Z",
         Col_Type => Values.Real_Type);

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

         begin
            Model.Add_Row
              ((T (Star_System.Name),
               R (Star_System.X), R (Star_System.Y), R (Star_System.Z)));
         end Add_Row;

      begin
         for Star_System of Concorde.Db.Star_System.Scan_By_Name loop
            Add_Row (Star_System);
         end loop;
      end;

   end Start;

end Concorde.UI.Models.Galaxy;
