private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Concorde.Color;
with Concorde.Star_Systems;

with Concorde.UI.Models.Tables;

with Concorde.Db;

package Concorde.UI.Models.Galaxy is

   type Root_Galaxy_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with private;

   function Star_Count
     (Model : Root_Galaxy_Model'Class)
      return Natural;

   function Star_Name
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return String
     with Pre => Index <= Model.Star_Count;

   function Star_X
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Real
     with Pre => Index <= Model.Star_Count;

   function Star_Y
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Real
     with Pre => Index <= Model.Star_Count;

   function Star_Z
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Real
     with Pre => Index <= Model.Star_Count;

   function Star_Color
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Concorde.Color.Concorde_Color
     with Pre => Index <= Model.Star_Count;

   function Star_Mass
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Non_Negative_Real
     with Pre => Index <= Model.Star_Count;

   function Star_Radius
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Non_Negative_Real
     with Pre => Index <= Model.Star_Count;

   function Star_Owner
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Concorde.Db.Faction_Reference;

   procedure Scan_Near_Systems
     (Model       : Root_Galaxy_Model'Class;
      Star_System : Concorde.Star_Systems.Star_System_Type'Class;
      Process     : not null access
        procedure (Near_System : Concorde.Star_Systems.Star_System_Type'Class;
                   Distance    : Non_Negative_Real));

   procedure Scan_Star_Systems
     (Model   : Root_Galaxy_Model'Class;
      Process : not null access
        procedure (Star_System : Concorde.Star_Systems.Star_System_Type'Class));

   function Find_Star
     (Model   : Root_Galaxy_Model'Class;
      X, Y, Z : Real)
      return Concorde.Db.Star_System_Reference;

   type Galaxy_Model is access all Root_Galaxy_Model'Class;

   function Create_Galaxy_Model
      return Galaxy_Model;

private

   type Near_System_Record is
      record
         Star_System : Concorde.Db.Star_System_Reference;
         Distance    : Non_Negative_Real;
      end record;

   package Near_System_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Near_System_Record);

   type Star_Record is
      record
         Reference    : Concorde.Db.Star_System_Reference;
         Star_System  : Concorde.Star_Systems.Star_System_Type;
         Name         : Ada.Strings.Unbounded.Unbounded_String;
         X, Y, Z      : Real;
         Color        : Concorde.Color.Concorde_Color;
         Mass         : Non_Negative_Real;
         Radius       : Non_Negative_Real;
         Luminosity   : Non_Negative_Real;
         Near_Systems : Near_System_Lists.List;
      end record;

   package Star_Vectors is
     new Ada.Containers.Vectors (Positive, Star_Record);

   type Root_Galaxy_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with
      record
         Vector : Star_Vectors.Vector;
      end record;

   overriding function Title
     (Model : Root_Galaxy_Model)
      return String;

   function Get_Star
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Star_Record
   is (Model.Vector.Element (Index))
     with Pre => Index <= Model.Vector.Last_Index;

   function Star_Count
     (Model : Root_Galaxy_Model'Class)
      return Natural
   is (Model.Vector.Last_Index);

   function Star_Name
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return String
   is (Ada.Strings.Unbounded.To_String
       (Model.Get_Star (Index).Name));

   function Star_X
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Real
   is (Model.Get_Star (Index).X);

   function Star_Y
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Real
   is (Model.Get_Star (Index).Y);

   function Star_Z
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Real
   is (Model.Get_Star (Index).Z);

   function Star_Color
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Concorde.Color.Concorde_Color
   is (Model.Get_Star (Index).Color);

   function Star_Mass
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Non_Negative_Real
   is (Model.Get_Star (Index).Mass);

   function Star_Radius
     (Model : Root_Galaxy_Model'Class;
      Index : Positive)
      return Non_Negative_Real
   is (Model.Get_Star (Index).Radius);

end Concorde.UI.Models.Galaxy;
