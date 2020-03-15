with Ada.Containers.Doubly_Linked_Lists;

with Nazar.Logging;

package body Nazar.Views.Gtk_Views.Layout is

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (View  : in out Nazar_Gtk_Layout_View_Record;
      Child : not null access Nazar_View_Record'Class)
   is
   begin
      View.Layout_Model.Attach
        (Child  => Child,
         Left   => Child.Get_Property ("attach-left", 0),
         Right  => Child.Get_Property ("attach-right", 1),
         Top    => Child.Get_Property ("attach-top", 0),
         Bottom => Child.Get_Property ("attach-bottom", 1));
   end Append;

   ----------------------------------
   -- Nazar_Gtk_Layout_View_Create --
   ----------------------------------

   function Nazar_Gtk_Layout_View_Create
     return Nazar_View
   is
   begin
      return Nazar_View
        (Nazar_Gtk_Layout_View_New
           (Nazar.Models.Layout.Layout_Model_New));
   end Nazar_Gtk_Layout_View_Create;

   -------------------------------
   -- Nazar_Gtk_Layout_View_New --
   -------------------------------

   function Nazar_Gtk_Layout_View_New
     (Model : not null access Nazar.Models.Layout.Root_Layout_Model'Class)
      return Nazar_Gtk_Layout_View
   is
      View : constant Nazar_Gtk_Layout_View :=
        new Nazar_Gtk_Layout_View_Record;
      Grid : constant Gtk.Grid.Gtk_Grid :=
        Gtk.Grid.Gtk_Grid_New;
   begin
      Grid.Set_Column_Homogeneous (True);
      Grid.Set_Column_Spacing (4);
      Grid.Set_Row_Homogeneous (True);
      Grid.Set_Row_Spacing (4);
      View.Initialize (Grid);
      View.Grid := Grid;
      View.Set_Model (Model);
      return View;
   end Nazar_Gtk_Layout_View_New;

   ----------------------
   -- Update_Container --
   ----------------------

   overriding procedure Update_Container
     (View   : in out Nazar_Gtk_Layout_View_Record;
      Update : not null access
        procedure (Container : in out Nazar.Views.Layout.Layout_Container))
   is
   begin
      Update (View.Layout);
   end Update_Container;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Layout_View_Record)
   is
      package List_Of_Views is
        new Ada.Containers.Doubly_Linked_Lists (Nazar_View);

      Removed_Views : List_Of_Views.List;

      procedure Check_View
        (Item : Nazar_View);

      procedure Check_Model
        (Item        : not null access Nazar_Object_Interface'Class;
         Left, Right : Natural;
         Top, Bottom : Natural);

      -----------------
      -- Check_Model --
      -----------------

      procedure Check_Model
        (Item        : not null access Nazar_Object_Interface'Class;
         Left, Right : Natural;
         Top, Bottom : Natural)
      is
         Child : constant Nazar_Gtk_View :=
           Nazar_Gtk_View (Item);
      begin
         if not View.Contains (Child) then
            Child.Widget.Set_Hexpand (True);
            Child.Widget.Set_Vexpand (True);
            View.Grid.Attach
              (Child  => Child.Widget,
               Left   => Glib.Gint (Left),
               Top    => Glib.Gint (Top),
               Width  => Glib.Gint (Right - Left),
               Height => Glib.Gint (Bottom - Top));
            View.Insert (Child);
            Child.Widget.Show_All;
         end if;
      end Check_Model;

      ----------------
      -- Check_View --
      ----------------

      procedure Check_View
        (Item : Nazar_View)
      is
      begin
         if not View.Layout_Model.Contains (Item) then
            Removed_Views.Append (Item);
         end if;
      end Check_View;

   begin
      Nazar.Logging.Log (View, "model changed");
      View.Iterate (Check_View'Access);

      for Child of Removed_Views loop
         View.Delete (Child);
      end loop;

      View.Layout_Model.Iterate_Children (Check_Model'Access);

   end Update_From_Model;

end Nazar.Views.Gtk_Views.Layout;
