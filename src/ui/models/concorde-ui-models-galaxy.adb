with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with Concorde.UI.Models.Data_Source;
with Concorde.UI.Models.Renderers;
with Concorde.UI.Models.Values;

with Concorde.Db.Star_System;

package body Concorde.UI.Models.Galaxy is

   type Galaxy_Row is
      record
         Star    : Concorde.Db.Star_System_Reference;
         X, Y, Z : Real;
      end record;

   package Galaxy_Table is
     new Ada.Containers.Vectors
       (Positive, Galaxy_Row);

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Galaxy_Column is
     (Star_Name, Star_X, Star_Y, Star_Z);

   type Galaxy_Model_Type is
     new Concorde.UI.Models.Data_Source.Root_Data_Source_Model with
      record
         Headings : String_Vectors.Vector;
         Ids      : String_Vectors.Vector;
         Data     : Galaxy_Table.Vector;
      end record;

   overriding procedure Start
     (Model     : in out Galaxy_Model_Type;
      Arguments : String);

   overriding function Name
     (Model : Galaxy_Model_Type)
      return String
   is ("galaxy-data-source");

   overriding function Column_Count
     (Model : Galaxy_Model_Type)
      return Natural
   is (Model.Ids.Last_Index);

   overriding function Row_Count
     (Model : Galaxy_Model_Type)
      return Natural
   is (Model.Data.Last_Index);

   overriding function Column_Heading_Id
     (Model       : Galaxy_Model_Type;
      Column      : Positive)
      return String
   is (Model.Ids.Element (Column));

   overriding function Column_Heading_Label
     (Model       : Galaxy_Model_Type;
      Column      : Positive)
      return String
   is (Model.Headings.Element (Column));

   overriding function Column_Type
     (Model       : Galaxy_Model_Type;
      Column      : Positive)
      return Values.Model_Value_Data_Type
   is (case Galaxy_Column'Val (Column - 1) is
          when Star_Name  =>
             Values.Text_Type,
          when Star_X | Star_Y | Star_Z =>
             Values.Real_Type);

   overriding function Column_Renderer
     (Model       : Galaxy_Model_Type;
      Column      : Positive)
      return Renderers.Render_Interface'Class
   is (case Galaxy_Column'Val (Column - 1) is
          when others => Renderers.Default_Renderer);

   overriding function Cell_Value
     (Model  : Galaxy_Model_Type;
      Row    : Positive;
      Column : Positive)
      return Values.Model_Value_Type;

   ----------------
   -- Cell_Value --
   ----------------

   overriding function Cell_Value
     (Model  : Galaxy_Model_Type;
      Row    : Positive;
      Column : Positive)
      return Values.Model_Value_Type
   is
      use Concorde.UI.Models.Values;
      Row_Rec : Galaxy_Row renames Model.Data (Row);
   begin
      case Galaxy_Column'Val (Column - 1) is
         when Star_Name =>
            return Text_Value
              (Concorde.Db.Star_System.Get (Row_Rec.Star).Name);
         when Star_X =>
            return Real_Value (Row_Rec.X);
         when Star_Y =>
            return Real_Value (Row_Rec.Y);
         when Star_Z =>
            return Real_Value (Row_Rec.Z);
      end case;
   end Cell_Value;

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
      Model.Ids.Clear;
      Model.Headings.Clear;
      Model.Data.Clear;

      Model.Ids.Append ("name");
      Model.Ids.Append ("system-x");
      Model.Ids.Append ("system-y");
      Model.Ids.Append ("system-z");

      Model.Headings.Append ("Name");
      Model.Headings.Append ("X");
      Model.Headings.Append ("Y");
      Model.Headings.Append ("Z");

      declare

         procedure Add_Row
           (Star_System : Concorde.Db.Star_System.Star_System_Type);

         -------------
         -- Add_Row --
         -------------

         procedure Add_Row
           (Star_System : Concorde.Db.Star_System.Star_System_Type)
         is
         begin
            Model.Data.Append
              (Galaxy_Row'
                 (Star => Star_System.Get_Star_System_Reference,
                  X    => Star_System.X,
                  Y    => Star_System.Y,
                  Z    => Star_System.Z));
         end Add_Row;

      begin
         for Star_System of Concorde.Db.Star_System.Scan_By_Name loop
            Add_Row (Star_System);
         end loop;
      end;

   end Start;

end Concorde.UI.Models.Galaxy;
