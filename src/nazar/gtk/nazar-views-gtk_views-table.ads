private with Gtk.List_Store;
private with Gtk.Scrolled_Window;
private with Gtk.Tree_View;

with Nazar.Models.Table;
with Nazar.Views.Table;

package Nazar.Views.Gtk_Views.Table is

   type Nazar_Gtk_Table_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Table.Nazar_Table_View_Interface
   with private;

   type Nazar_Gtk_Table_View is access all Nazar_Gtk_Table_View_Record'Class;

   function Nazar_Gtk_Table_View_New
     (Model : not null access Nazar.Models.Table
      .Nazar_Table_Model_Record'Class)
      return Nazar_Gtk_Table_View;

   function Nazar_Gtk_Table_View_Create
      return Nazar_View;

private

   type Nazar_Gtk_Table_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Table.Nazar_Table_View_Interface with
      record
         List_Model : Gtk.List_Store.Gtk_List_Store;
         Tree_View  : Gtk.Tree_View.Gtk_Tree_View;
         Scrolled   : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      end record;

   overriding function Class_Name
     (Table : Nazar_Gtk_Table_View_Record)
      return String
   is ("nazar-gtk-table-view");

   overriding procedure Model_Changed
     (View : in out Nazar_Gtk_Table_View_Record);

   type Model_Access is
     access all Nazar.Models.Table.Nazar_Table_Model_Record'Class;

   function Table_Model
     (View : Nazar_Gtk_Table_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Table;
