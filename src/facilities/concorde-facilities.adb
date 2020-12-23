with WL.String_Maps;

with Concorde.Handles.Facility;
with Concorde.Handles.Facility_Class;

package body Concorde.Facilities is

   package Facility_Class_Maps is
     new WL.String_Maps
       (Concorde.Handles.Facility_Class_Reference, Concorde.Handles."=");

   Map : Facility_Class_Maps.Map;

   --------------------
   -- Facility_Class --
   --------------------

   function Facility_Class
     (Name : String)
      return Concorde.Handles.Facility_Class_Reference
   is
      Position : constant Facility_Class_Maps.Cursor := Map.Find (Name);
   begin
      if Facility_Class_Maps.Has_Element (Position) then
         return Facility_Class_Maps.Element (Position);
      else
         declare
            use type Concorde.Handles.Facility_Class_Reference;
            Reference : constant Concorde.Handles.Facility_Class_Reference :=
                          Concorde.Handles.Facility_Class.Get_By_Tag
                            (Name);
         begin
            if Reference /= Concorde.Handles.Null_Facility_Class_Reference then
               return Reference;
            else
               return Concorde.Handles.Facility_Class.Create (Name);
            end if;
         end;
      end if;
   end Facility_Class;

   ---------
   -- Get --
   ---------

   function Get
     (Tag : String)
      return Concorde.Handles.Facility_Reference
   is
   begin
      return Concorde.Handles.Facility.Get_By_Tag (Tag);
   end Get;

end Concorde.Facilities;
