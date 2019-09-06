private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Concorde.Db;

with Concorde.Ships.Lists;

with Concorde.UI.Models.Tables;

package Concorde.UI.Models.Star_System is

   type Root_Star_System_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with private;

   function Star_System
     (Model : Root_Star_System_Model'Class)
      return Concorde.Db.Star_System_Reference;

   function World_Count
     (Model : Root_Star_System_Model'Class)
      return Natural;

   function Reference
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Concorde.Db.World_Reference;

   function Name
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return String;

   function Category
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Concorde.Db.World_Category;

   function Climate
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Concorde.Db.Climate_Reference;

   function Radius
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real;
   --  relative to Earth radius

   function Orbit
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real;
   --  relative to Earth orbit; i.e. in AU

   function Current_Longitude
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real;
   --  in degrees

   function Find_World
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real)
      return Concorde.Db.World_Reference;

   function Find_World_Index
     (Model     : Root_Star_System_Model'Class;
      Orbit     : Non_Negative_Real;
      Longitude : Real)
      return Natural;

   type Star_System_Model is
     access all Root_Star_System_Model'Class;

   function Create
     (Star_System : Concorde.Db.Star_System_Reference)
      return Star_System_Model;

private

   type World_Record is
      record
         Reference       : Concorde.Db.World_Reference;
         Name            : Ada.Strings.Unbounded.Unbounded_String;
         Category        : Concorde.Db.World_Category;
         Climate         : Concorde.Db.Climate_Reference;
         Radius          : Non_Negative_Real;
         Orbit           : Non_Negative_Real;
         Year            : Non_Negative_Real;
         Start_Longitude : Non_Negative_Real;
      end record;

   package World_Vectors is
     new Ada.Containers.Vectors (Positive, World_Record);

   type Root_Star_System_Model is
     new Concorde.UI.Models.Tables.Root_Table_Model with
      record
         Star_System : Concorde.Db.Star_System_Reference;
         Vector      : World_Vectors.Vector;
         Ships       : Concorde.Ships.Lists.List;
      end record;

   function Star_System
     (Model : Root_Star_System_Model'Class)
      return Concorde.Db.Star_System_Reference
   is (Model.Star_System);

   function World_Count
     (Model : Root_Star_System_Model'Class)
      return Natural
   is (Model.Vector.Last_Index);

   function Reference
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Concorde.Db.World_Reference
   is (Model.Vector.Element (Index).Reference);

   function Name
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return String
   is (Ada.Strings.Unbounded.To_String (Model.Vector.Element (Index).Name));

   function Category
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Concorde.Db.World_Category
   is (Model.Vector.Element (Index).Category);

   function Climate
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Concorde.Db.Climate_Reference
   is (Model.Vector.Element (Index).Climate);

   function Radius
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real
   is (Model.Vector.Element (Index).Radius);

   function Orbit
     (Model : Root_Star_System_Model'Class;
      Index : Positive)
      return Non_Negative_Real
   is (Model.Vector.Element (Index).Orbit);

end Concorde.UI.Models.Star_System;
