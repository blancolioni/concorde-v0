with Ada.Containers.Doubly_Linked_Lists;

package body Nazar.Views.Gtk_Views.Layout is

   ---------------------
   -- Gtk_Layout_View --
   ---------------------

   function Gtk_Layout_View
     (Model : not null access Nazar.Models.Layout.Root_Layout_Model'Class)
      return Nazar_Gtk_Layout_View
   is
      View : constant Nazar_Gtk_Layout_View :=
        new Root_Gtk_Layout_View;
      Grid : constant Gtk.Grid.Gtk_Grid :=
        Gtk.Grid.Gtk_Grid_New;
   begin
      View.Initialize (Grid);
      View.Set_Model (Model);
      return View;
   end Gtk_Layout_View;

   overriding procedure Model_Changed
     (View : in out Root_Gtk_Layout_View)
   is
      package List_Of_Views is
        new Ada.Containers.Doubly_Linked_Lists (View_Type);

      Removed_Views : List_Of_Views.List;

      procedure Check_View
        (Item : View_Type);

      procedure Check_Model
        (Item : not null access Nazar_Object_Interface'Class;
         Left, Right : Natural;
         Top, Bottom : Natural);

      procedure Check_Model
        (Item        : not null access Nazar_Object_Interface'Class;
         Left, Right : Natural;
         Top, Bottom : Natural)
      is
         Child : constant Nazar_Gtk_View :=
           Nazar_Gtk_View (Item);
      begin
         if not View.Contains (Child) then
            View.Grid.Attach
              (Child  => Child.Widget,
               Left   => Glib.Gint (Left),
               Top    => Glib.Gint (Top),
               Width  => Glib.Gint (Right - Left),
               Height => Glib.Gint (Bottom - Top));
            View.Insert (Child);
         end if;
      end Check_Model;

      ----------------
      -- Check_View --
      ----------------

      procedure Check_View
        (Item : View_Type)
      is
      begin
         if not View.Layout_Model.Contains (Item) then
            Removed_Views.Append (Item);
         end if;
      end Check_View;

   begin
      View.Iterate (Check_View'Access);

      for Child of Removed_Views loop
         View.Delete (Child);
      end loop;

      View.Layout_Model.Iterate_Children (Check_Model'Access);

   end Model_Changed;

   ----------------------
   -- Update_Container --
   ----------------------

   overriding procedure Update_Container
     (View   : in out Root_Gtk_Layout_View;
      Update : not null access
        procedure (Container : in out Nazar.Views.Layout.Layout_Container))
   is
   begin
      Update (View.Layout);
   end Update_Container;

end Nazar.Views.Gtk_Views.Layout;
