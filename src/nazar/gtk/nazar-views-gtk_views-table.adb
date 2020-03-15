with Glib;

with Gtk.Enums;

with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;
with Gtk.Tree_View_Column;

with Nazar.Interfaces.Table;

package body Nazar.Views.Gtk_Views.Table is

   function To_Gtk_Column_Type
     (Item : Nazar.Values.Nazar_Value_Type)
      return Glib.GType;

   ---------------------------------
   -- Nazar_Gtk_Table_View_Create --
   ---------------------------------

   function Nazar_Gtk_Table_View_Create return Nazar_View is
      View : constant Nazar_Gtk_Table_View :=
               new Nazar_Gtk_Table_View_Record;
   begin
      View.List_Model := null;
      View.Tree_View :=
        Gtk.Tree_View.Gtk_Tree_View_New_With_Model
          (Gtk.List_Store.Implements_Gtk_Tree_Model.To_Interface
             (View.List_Model));
      View.Scrolled := Gtk.Scrolled_Window.Gtk_Scrolled_Window_New;
      View.Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      View.Scrolled.Add (View.Tree_View);
      View.Initialize (View.Scrolled);
      return Nazar_View (View);
   end Nazar_Gtk_Table_View_Create;

   ------------------------------
   -- Nazar_Gtk_Table_View_New --
   ------------------------------

   function Nazar_Gtk_Table_View_New
     (Model : not null access Nazar.Models.Table
      .Nazar_Table_Model_Record'Class)
      return Nazar_Gtk_Table_View
   is
      View : constant Nazar_Gtk_Table_View :=
               Nazar_Gtk_Table_View (Nazar_Gtk_Table_View_Create);
   begin
      View.Set_Model (Model);
      return View;
   end Nazar_Gtk_Table_View_New;

   ------------------------
   -- To_Gtk_Column_Type --
   ------------------------

   function To_Gtk_Column_Type
     (Item : Nazar.Values.Nazar_Value_Type)
      return Glib.GType
   is
      use Glib;
      use Nazar.Values;
   begin
      if Item = Boolean_Value_Type then
         return GType_Boolean;
      elsif Item = Integer_Value_Type then
         return GType_Int;
      elsif Item = Real_Value_Type then
         return GType_Double;
      elsif Item = Text_Value_Type then
         return GType_String;
      else
         return GType_String;
      end if;
   end To_Gtk_Column_Type;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Table_View_Record)
   is
      use Glib;
      use Gtk.List_Store;
      Model          : constant Model_Access := View.Table_Model;
      Store_Types    : GType_Array
        (1 .. Guint (View.Table_Model.Column_Count));
      Text_Render    : constant Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text
        := Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text_New;
      Layout_Changed : Boolean := View.List_Model = null;
   begin

      for I in 1 .. Model.Column_Count loop
         declare
            use type Nazar.Values.Nazar_Value_Type;
            Column_Type : constant Nazar.Values.Nazar_Value_Type :=
              Model.Column_Type (I);
         begin
            if I > View.Column_Cache.Last_Index
              or else View.Column_Cache.Element (I).Data_Type /= Column_Type
            then
               Layout_Changed := True;
            end if;

            Store_Types (Guint (I)) :=
              To_Gtk_Column_Type (Model.Column_Type (I));
         end;
      end loop;

      if View.List_Model = null then
         View.List_Model :=
           Gtk.List_Store.Gtk_List_Store_Newv (Store_Types);
         View.Tree_View.Set_Model
           (Gtk.List_Store.Implements_Gtk_Tree_Model.To_Interface
              (View.List_Model));
      elsif Layout_Changed then
         View.List_Model.Clear;
         View.List_Model.Set_Column_Types (Store_Types);
      end if;

      if Layout_Changed then
         View.Column_Cache.Clear;
         for I in 1 .. Model.Column_Count loop
            View.Column_Cache.Append
              (View_Column'
                 (Data_Type  => Model.Column_Type (I)));
         end loop;
      end if;

      declare
         use Nazar.Interfaces.Table;
         Row  : Row_Cursor_Interface'Class := Model.First_Row;
         Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      begin
         if not Layout_Changed then
            Iter := Gtk.Tree_Model.Get_Iter_First (+View.List_Model);
         end if;

         while Row.Has_Element loop
            declare
               use Glib;
               use type Gtk.List_Store.Gtk_List_Store;
               use type Gtk.Tree_Model.Gtk_Tree_Iter;
               Cell  : Cell_Cursor_Interface'Class := Row.First_Cell;
               Index : Gint := 0;
               New_Row : constant Boolean := Iter = Gtk.Tree_Model.Null_Iter;
            begin
               if New_Row then
                  View.List_Model.Append (Iter);
               end if;

               while Cell.Has_Element loop
                  declare
                     Text  : constant String :=
                       Nazar.Values.To_String (Cell.Value);
                  begin
                     View.List_Model.Set (Iter, Index, Text);
                     Cell.Next;
                     Index := Index + 1;
                  end;
               end loop;

               Gtk.Tree_Model.Next (+View.List_Model, Iter);

            end;
            Row.Next;
         end loop;
      end;

      if Layout_Changed then
         declare
            use Gtk.Tree_View_Column;
         begin
            while View.Tree_View.Get_Column (0) /= null loop
               declare
                  Index : constant Glib.Gint :=
                    View.Tree_View.Remove_Column
                      (View.Tree_View.Get_Column (0));
               begin
                  pragma Unreferenced (Index);
               end;
            end loop;

            for I in 1 .. Model.Column_Count loop
               declare
                  Column : constant Gtk_Tree_View_Column :=
                    Gtk_Tree_View_Column_New;
                  Index  : constant Glib.Gint :=
                    View.Tree_View.Append_Column (Column);
               begin
                  pragma Unreferenced (Index);
                  Column.Pack_Start (Text_Render, True);
                  Column.Set_Title (Model.Column_Heading (I));
                  Column.Add_Attribute
                    (Text_Render, "text", Glib.Gint (I - 1));
               end;
            end loop;
         end;
      end if;
   end Update_From_Model;

end Nazar.Views.Gtk_Views.Table;
