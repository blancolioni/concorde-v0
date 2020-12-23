with Concorde.Handles.Property;
with Concorde.Handles.Property_Entry;

package body Concorde.Properties is

   package Property_Reference_Maps is
     new WL.String_Maps
       (Concorde.Handles.Property_Reference, Concorde.Handles."=");

   Map : Property_Reference_Maps.Map;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (List : in out Property_List;
      Ref  : Concorde.Handles.Has_Properties_Reference)
   is
   begin
      List.Has_Properties := Ref;
      List.Map.Clear;
   end Create_List;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (List     : in out Property_List;
      Property : Property_Type)
      return Real
   is
   begin
      return List.Get_Property_Entry (Property).Value;
   end Get_Property;

   ------------------------
   -- Get_Property_Entry --
   ------------------------

   function Get_Property_Entry
     (List     : in out Property_List;
      Property : Property_Type)
      return Property_List_Entry
   is
      Position : constant Property_Maps.Cursor :=
        List.Map.Find (String (Property));
   begin
      if Property_Maps.Has_Element (Position) then
         return Property_Maps.Element (Position);
      else
         declare
            Property_Ref         : constant Concorde.Handles.Property_Reference :=
              Get_Reference (String (Property));
            Prop_Entry           : constant Concorde.Handles.Property_Entry
              .Property_Entry_Type :=
                Concorde.Handles.Property_Entry.Get_By_Property_Entry
                  (List.Has_Properties, Property_Ref);
            New_Entry            : constant Property_List_Entry :=
              (if Prop_Entry.Has_Element
               then (True, Prop_Entry.Value)
               else (False, 0.0));
         begin
            List.Map.Insert (String (Property), New_Entry);
            return New_Entry;
         end;
      end if;
   end Get_Property_Entry;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference
     (Name : String)
      return Concorde.Handles.Property_Reference
   is
      Position : constant Property_Reference_Maps.Cursor := Map.Find (Name);
   begin
      if Property_Reference_Maps.Has_Element (Position) then
         return Property_Reference_Maps.Element (Position);
      else
         declare
            use type Concorde.Handles.Property_Reference;
            Reference : constant Concorde.Handles.Property_Reference :=
                          Concorde.Handles.Property.Get_By_Tag (Name);
         begin
            if Reference /= Concorde.Handles.Null_Property_Reference then
               return Reference;
            else
               return Concorde.Handles.Property.Create (Name);
            end if;
         end;
      end if;
   end Get_Reference;

   ------------------
   -- Has_Property --
   ------------------

   function Has_Property
     (List     : in out Property_List;
      Property : Property_Type)
      return Boolean
   is
   begin
      return List.Get_Property_Entry (Property).Exists;
   end Has_Property;

end Concorde.Properties;
