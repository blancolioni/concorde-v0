package body Nazar.Views.Layout is

   --------------
   -- Contains --
   --------------

   function Contains
     (Layout : Layout_View_Interface'Class;
      Item   : not null access constant Root_View_Type'Class)
      return Boolean
   is
      use type WL.Guids.Guid;
   begin
      for Child of Layout.Container.Contents loop
         if Child.View /= null
           and then Child.View.Guid = Item.Guid
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Layout : in out Layout_View_Interface'Class;
      Item   : not null access Root_View_Type'Class)
   is
      use type WL.Guids.Guid;

      Position : Cell_Content_Lists.Cursor :=
        Cell_Content_Lists.No_Element;

      procedure Delete (Container : in out Layout_Container);

   begin
      for It in Layout.Container.Contents.Iterate loop
         if Cell_Content_Lists.Element (It).View.Guid
           = Item.Guid
         then
            Position := It;
            exit;
         end if;
      end loop;

      Layout.Update_Container (Delete'Access);

   end Delete;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Layout : in out Layout_View_Interface'Class;
      Item   :        not null access Root_View_Type'Class)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Layout  : Layout_View_Interface'Class;
      Process : not null access procedure (Item : View_Type))
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Iterate unimplemented");
      raise Program_Error with "Unimplemented procedure Iterate";
   end Iterate;

   ----------------------
   -- Set_Column_Width --
   ----------------------

   procedure Set_Column_Width
     (Layout       : in out Layout_View_Interface'Class;
      Column_Index : Positive;
      Width        : Nazar.Measurements.Nazar_Measurement)
   is
      procedure Set (Container : in out Layout_Container);

   begin
      Layout.Update_Container (Set'Access);
   end Set_Column_Width;

   --------------------
   -- Set_Row_Height --
   --------------------

   procedure Set_Row_Height
     (Layout : in out Layout_View_Interface'Class; Row_Index : Positive;
      Height :        Nazar.Measurements.Nazar_Measurement)
   is
      procedure Set (Container : in out Layout_Container);

   begin
      Layout.Update_Container (Set'Access);
   end Set_Row_Height;

end Nazar.Views.Layout;
