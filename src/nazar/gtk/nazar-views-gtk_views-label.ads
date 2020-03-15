with Gtk.Label;

with Nazar.Models.Text;
with Nazar.Views.Label;

package Nazar.Views.Gtk_Views.Label is

   type Nazar_Gtk_Label_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Label.Label_View_Interface
   with private;

   type Nazar_Gtk_Label_View is access all Nazar_Gtk_Label_View_Record'Class;

   function Nazar_Gtk_Label_View_New
     (Model : not null access Nazar.Models.Text.Nazar_Text_Model_Record'Class)
      return Nazar_Gtk_Label_View;

   function Nazar_Gtk_Label_View_New
     (Text : String)
      return Nazar_Gtk_Label_View;

   function Nazar_Gtk_Label_View_Create
      return Nazar_View;

private

   type Nazar_Gtk_Label_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Label.Label_View_Interface with
      record
         Label : Gtk.Label.Gtk_Label;
      end record;

   overriding function Class_Name
     (View : Nazar_Gtk_Label_View_Record)
      return String
   is ("nazar-gtk-label-view");

   overriding procedure Declare_Properties
     (View : in out Nazar_Gtk_Label_View_Record);

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Label_View_Record);

   type Model_Access is
     access all Nazar.Models.Text.Nazar_Text_Model_Record'Class;

   function Text_Model
     (View : Nazar_Gtk_Label_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Label;
