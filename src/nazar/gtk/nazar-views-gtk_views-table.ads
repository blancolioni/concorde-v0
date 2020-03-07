with Nazar.Interfaces.Table;
with Nazar.Views.Table;

package Nazar.Views.Gtk_Views.Table is

   type Nazar_Gtk_Table_View_Recrd is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Tables.Table_View_Interface
   with private;

   type Nazar_Gtk_Table_View is access all Root_Gtk_Table_View'Class;

   function Gtk_Table_View
     (Model : not null access Nazar.Models.Tables.Table_Model_Interface'Class)
      return Nazar_Gtk_Table_View;

private

   type Root_Gtk_Table_View is
     new Root_Gtk_View_Type
     and Nazar.Views.Tables.Table_View_Interface with
      record
         null;
      end record;

   overriding procedure Model_Changed
     (View : in out Root_Gtk_Table_View);

   type Model_Access is
     access all Nazar.Models.Tables.Table_Model_Interface'Class;

   function Table_Model
     (View : Root_Gtk_Table_View'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Tables;
