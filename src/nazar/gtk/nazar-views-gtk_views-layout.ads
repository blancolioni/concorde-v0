private with Gtk.Grid;

with Nazar.Models.Layout;
with Nazar.Views.Layout;

package Nazar.Views.Gtk_Views.Layout is

   type Nazar_Gtk_Layout_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Layout.Layout_View_Interface
   with private;

   type Nazar_Gtk_Layout_View is access all Nazar_Gtk_Layout_View_Record'Class;

   function Nazar_Gtk_Layout_View_New
     (Model : not null access Nazar.Models.Layout.Root_Layout_Model'Class)
      return Nazar_Gtk_Layout_View;

   function Nazar_Gtk_Layout_View_Create
      return Nazar_View;

private

   type Nazar_Gtk_Layout_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Layout.Layout_View_Interface with
      record
         Layout : Nazar.Views.Layout.Layout_Container;
         Grid   : Gtk.Grid.Gtk_Grid;
      end record;

   overriding function Class_Name
     (View : Nazar_Gtk_Layout_View_Record)
      return String
   is ("nazar-gtk-layout-view");

   overriding function Container
     (View : Nazar_Gtk_Layout_View_Record)
      return Nazar.Views.Layout.Layout_Container
   is (View.Layout);

   overriding procedure Update_Container
     (View : in out Nazar_Gtk_Layout_View_Record;
      Update : not null access
        procedure (Container : in out Nazar.Views.Layout.Layout_Container));

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Layout_View_Record);

   overriding procedure Append
     (View  : in out Nazar_Gtk_Layout_View_Record;
      Child : not null access Nazar_View_Record'Class);

   type Model_Access is
     access all Nazar.Models.Layout.Root_Layout_Model'Class;

   function Layout_Model
     (View : Nazar_Gtk_Layout_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Layout;
