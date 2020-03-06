with Nazar.Models.Text;
with Nazar.Views.Labels;

package Nazar.Views.Gtk_Views.Labels is

   type Root_Gtk_Label_View is
     new Root_Gtk_View_Type
     and Nazar.Views.Labels.Label_View_Interface
   with private;

   type Nazar_Gtk_Label_View is access all Root_Gtk_Label_View'Class;

   function Gtk_Label_View
     (Model : not null access Nazar.Models.Text.Root_Text_Model'Class)
      return Nazar_Gtk_Label_View;

private

   type Root_Gtk_Label_View is
     new Root_Gtk_View_Type
     and Nazar.Views.Labels.Label_View_Interface with
      record
         null;
      end record;

   overriding function Class_Name
     (View : Root_Gtk_Label_View)
      return String
   is ("nazar-gtk-label-view");

   overriding procedure Model_Changed
     (View : in out Root_Gtk_Label_View);

   type Model_Access is
     access all Nazar.Models.Text.Root_Text_Model'Class;

   function Text_Model
     (View : Root_Gtk_Label_View'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Labels;
