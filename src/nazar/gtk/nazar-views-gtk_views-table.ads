private with Ada.Containers.Vectors;

private with Gtk.List_Store;
private with Gtk.Scrolled_Window;
private with Gtk.Tree_View;

private with Nazar.Values;

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

   type View_Column is
      record
         Data_Type  : Nazar.Values.Nazar_Value_Type;
      end record;

   package View_Column_Vectors is
     new Ada.Containers.Vectors (Positive, View_Column);

   type Cell_Record is
      record
         Data : Nazar.Values.Nazar_Value;
      end record;

   package View_Cell_Vectors is
     new Ada.Containers.Vectors (Positive, Cell_Record);

   package View_Row_Vectors is
     new Ada.Containers.Vectors (Positive, View_Cell_Vectors.Vector,
                                 View_Cell_Vectors."=");

   type Nazar_Gtk_Table_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Table.Nazar_Table_View_Interface with
      record
         List_Model   : Gtk.List_Store.Gtk_List_Store;
         Tree_View    : Gtk.Tree_View.Gtk_Tree_View;
         Scrolled     : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
         Column_Cache : View_Column_Vectors.Vector;
         Value_Cache  : View_Row_Vectors.Vector;
      end record;

   overriding function Class_Name
     (Table : Nazar_Gtk_Table_View_Record)
      return String
   is ("nazar-gtk-table-view");

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Table_View_Record);

   type Model_Access is
     access all Nazar.Models.Table.Nazar_Table_Model_Record'Class;

   function Table_Model
     (View : Nazar_Gtk_Table_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Table;
