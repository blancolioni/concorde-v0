private with Gtk.Box;

with Nazar.Models.Layout;
with Nazar.Views.Box;
with Nazar.Views.Layout;
with Nazar.Views.Orientable;

package Nazar.Views.Gtk_Views.Box is

   type Nazar_Gtk_Box_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Box.Box_View_Interface
   with private;

   type Nazar_Gtk_Box_View is
     access all Nazar_Gtk_Box_View_Record'Class;

   function Nazar_Gtk_Box_View_New
     (Model : not null access Nazar.Models.Layout.Root_Layout_Model'Class;
      Orientation : Nazar.Views.Orientable.Nazar_Orientation)
      return Nazar_Gtk_Box_View;

   function Nazar_Gtk_Box_View_Create
     (Orientation : Nazar.Views.Orientable.Nazar_Orientation)
      return Nazar_View;

private

   type Nazar_Gtk_Box_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Box.Box_View_Interface with
      record
         Box         : Gtk.Box.Gtk_Box;
         Orientation : Nazar.Views.Orientable.Nazar_Orientation;
      end record;

   overriding function Class_Name
     (View : Nazar_Gtk_Box_View_Record)
      return String
   is ("nazar-gtk-box-view");

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Box_View_Record);

   overriding function Orientation
     (View : Nazar_Gtk_Box_View_Record)
      return Nazar.Views.Orientable.Nazar_Orientation
   is (View.Orientation);

   overriding procedure Set_Orientation
     (View        : in out Nazar_Gtk_Box_View_Record;
      Orientation : Nazar.Views.Orientable.Nazar_Orientation);

   overriding procedure Append
     (View  : in out Nazar_Gtk_Box_View_Record;
      Child : not null access Nazar_View_Record'Class);

   type Model_Access is
     access all Nazar.Models.Layout.Root_Layout_Model'Class;

   function Layout_Model
     (View : Nazar_Gtk_Box_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Box;
