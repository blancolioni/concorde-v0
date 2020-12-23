private with WL.String_Maps;

with Concorde.Db;

package Concorde.Properties is

   type Property_List is tagged private;

   type Property_Type (<>) is private;

   function Property (Name : String) return Property_Type;

   procedure Create_List
     (List : in out Property_List;
      Ref  : Concorde.Handles.Has_Properties_Reference);

   function Has_Property
     (List     : in out Property_List;
      Property : Property_Type)
      return Boolean;

   function Get_Property
     (List     : in out Property_List;
      Property : Property_Type)
      return Real;

   function Get_Reference
     (Name : String)
      return Concorde.Handles.Property_Reference;

private

   type Property_List_Entry is
      record
         Exists : Boolean;
         Value  : Real;
      end record;

   package Property_Maps is
     new WL.String_Maps (Property_List_Entry);

   type Property_List is tagged
      record
         Has_Properties : Concorde.Handles.Has_Properties_Reference;
         Map            : Property_Maps.Map;
      end record;

   function Get_Property_Entry
     (List     : in out Property_List;
      Property : Property_Type)
      return Property_List_Entry;

   type Property_Type is new String;

   function Property (Name : String) return Property_Type
   is (Property_Type (Name));

end Concorde.Properties;
