private with Gtk.Window;

with Nazar.Models.Application;
with Nazar.Views.Application;

package Nazar.Views.Gtk_Views.Application is

   type Nazar_Gtk_Application_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Application.Nazar_Application_View_Interface
   with private;

   type Nazar_Gtk_Application_View is
     access all Nazar_Gtk_Application_View_Record'Class;

   function Nazar_Gtk_Application_View_New
     (Model : not null access
        Nazar.Models.Application.Nazar_Application_Model_Record'Class)
      return Nazar_Gtk_Application_View;

   function Nazar_Gtk_Application_View_Create
     return Nazar.Views.Nazar_View;

private

   type Nazar_Gtk_Application_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Application.Nazar_Application_View_Interface with
      record
         Window : Gtk.Window.Gtk_Window;
      end record;

   overriding function Class_Name
     (View : Nazar_Gtk_Application_View_Record)
      return String
   is ("nazar-gtk-application-view");

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Application_View_Record);

   overriding procedure Append
     (View  : in out Nazar_Gtk_Application_View_Record;
      Child : not null access Nazar.Views.Nazar_View_Record'Class);

   overriding procedure Show
     (View  : in out Nazar_Gtk_Application_View_Record);

   overriding procedure Declare_Properties
     (View  : in out Nazar_Gtk_Application_View_Record);

   type Model_Access is
     access all Nazar.Models.Application.Nazar_Application_Model_Record'Class;

   function Application_Model
     (View : Nazar_Gtk_Application_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Application;
