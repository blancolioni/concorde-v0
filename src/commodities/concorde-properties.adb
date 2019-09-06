with WL.String_Maps;

with Concorde.Db.Property;

package body Concorde.Properties is

   package Property_Maps is
     new WL.String_Maps
       (Concorde.Db.Property_Reference, Concorde.Db."=");

   Map : Property_Maps.Map;

   --------------
   -- Property --
   --------------

   function Property
     (Name : String)
      return Concorde.Db.Property_Reference
   is
      Position : constant Property_Maps.Cursor := Map.Find (Name);
   begin
      if Property_Maps.Has_Element (Position) then
         return Property_Maps.Element (Position);
      else
         declare
            use type Concorde.Db.Property_Reference;
            Reference : constant Concorde.Db.Property_Reference :=
                          Concorde.Db.Property.Get_Reference_By_Tag (Name);
         begin
            if Reference /= Concorde.Db.Null_Property_Reference then
               return Reference;
            else
               return Concorde.Db.Property.Create (Name);
            end if;
         end;
      end if;
   end Property;

end Concorde.Properties;
