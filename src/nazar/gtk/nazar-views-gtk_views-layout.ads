private with Gtk.Grid;

with Nazar.Models.Layout;
with Nazar.Views.Layout;

package Nazar.Views.Gtk_Views.Layout is

   type Root_Gtk_Layout_View is
     new Root_Gtk_View_Type
     and Nazar.Views.Layout.Layout_View_Interface
   with private;

   type Nazar_Gtk_Layout_View is access all Root_Gtk_Layout_View'Class;

   function Gtk_Layout_View
     (Model : not null access Nazar.Models.Layout.Root_Layout_Model'Class)
      return Nazar_Gtk_Layout_View;

private

   type Root_Gtk_Layout_View is
     new Root_Gtk_View_Type
     and Nazar.Views.Layout.Layout_View_Interface with
      record
         Layout : Nazar.Views.Layout.Layout_Container;
         Grid   : Gtk.Grid.Gtk_Grid;
      end record;

   overriding function Class_Name
     (View : Root_Gtk_Layout_View)
      return String
   is ("nazar-gtk-layout-view");

   overriding function Container
     (View : Root_Gtk_Layout_View)
      return Nazar.Views.Layout.Layout_Container
   is (View.Layout);

   overriding procedure Update_Container
     (View : in out Root_Gtk_Layout_View;
      Update : not null access
        procedure (Container : in out Nazar.Views.Layout.Layout_Container));

   overriding procedure Model_Changed
     (View : in out Root_Gtk_Layout_View);

   type Model_Access is
     access all Nazar.Models.Layout.Root_Layout_Model'Class;

   function Layout_Model
     (View : Root_Gtk_Layout_View'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Layout;
