with Glib.Object;
with Gtk.Widget;

package Nazar.Views.Gtk_Views is

   type Nazar_Gtk_View_Record is abstract new Nazar_View_Record with private;

   overriding procedure Show (View : in out Nazar_Gtk_View_Record);

   overriding procedure Model_Changed
     (View : in out Nazar_Gtk_View_Record);

   procedure Update_From_Model
     (View : in out Nazar_Gtk_View_Record)
   is abstract;

   function Widget
     (View : Nazar_Gtk_View_Record)
      return Gtk.Widget.Gtk_Widget;

   function Object
     (View : Nazar_Gtk_View_Record)
      return Glib.Object.GObject;

   type Nazar_Gtk_View is access all Nazar_Gtk_View_Record'Class;

   procedure Initialize
     (View : not null access Nazar_Gtk_View_Record'Class;
      Top  : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   generic
      type Concrete_View_Record is new Nazar_Gtk_View_Record with private;
      type Concrete_View_Access is access all Concrete_View_Record'Class;
   function From_Gtk_Object
     (Object : not null access Glib.Object.GObject_Record'Class)
      return Concrete_View_Access;

private

   type Gtk_View_Object_Record is
     new Glib.Object.GObject_Record with
      record
         View : Nazar_Gtk_View;
      end record;

   type Gtk_View_Object is access all Gtk_View_Object_Record'Class;

   type Nazar_Gtk_View_Record is abstract new Nazar_View_Record with
      record
         Self       : Gtk_View_Object;
         Top_Widget : Gtk.Widget.Gtk_Widget;
      end record;

   overriding procedure Set_Name
     (View  : in out Nazar_Gtk_View_Record;
      Name  : String);

   function From_Gtk_Object
     (Object : not null access Glib.Object.GObject_Record'Class)
      return Concrete_View_Access
   is (Concrete_View_Access (Gtk_View_Object_Record (Object.all).View));

   function Object
     (View : Nazar_Gtk_View_Record)
      return Glib.Object.GObject
   is (Glib.Object.GObject (View.Self));

end Nazar.Views.Gtk_Views;
