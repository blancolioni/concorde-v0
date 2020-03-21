with Gtk.Scale;

with Nazar.Models.Numeric;
with Nazar.Views.Scale;

package Nazar.Views.Gtk_Views.Scale is

   type Nazar_Gtk_Scale_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Scale.Nazar_Scale_View_Interface
   with private;

   type Nazar_Gtk_Scale_View is access all Nazar_Gtk_Scale_View_Record'Class;

   function Nazar_Gtk_Scale_View_New
     (Model : not null access
        Nazar.Models.Numeric.Nazar_Float_Model_Record'Class)
      return Nazar_Gtk_Scale_View;

   function Nazar_Gtk_Scale_View_Create
      return Nazar_View;

private

   type Nazar_Gtk_Scale_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Scale.Nazar_Scale_View_Interface with
      record
         Scale : Gtk.Scale.Gtk_Scale;
      end record;

   overriding function Class_Name
     (View : Nazar_Gtk_Scale_View_Record)
      return String
   is ("nazar-gtk-scale-view");

--     overriding procedure Declare_Properties
--       (View : in out Nazar_Gtk_Scale_View_Record);

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Scale_View_Record);

   type Model_Access is
     access all Nazar.Models.Numeric.Nazar_Float_Model_Record'Class;

   function Scale_Model
     (View : Nazar_Gtk_Scale_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Scale;
