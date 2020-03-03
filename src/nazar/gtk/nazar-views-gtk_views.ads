with Glib.Object;
with Gtk.Widget;

package Nazar.Views.Gtk_Views is

   type Root_Gtk_View_Type is abstract new Root_View_Type with private;

   overriding procedure Show (View : in out Root_Gtk_View_Type);

   function Widget
     (View : Root_Gtk_View_Type)
      return Gtk.Widget.Gtk_Widget;

   function Object
     (View : Root_Gtk_View_Type)
      return Glib.Object.GObject;

   type Nazar_Gtk_View is access all Root_Gtk_View_Type'Class;

   procedure Initialize
     (View : not null access Root_Gtk_View_Type'Class;
      Top  : not null access Gtk.Widget.Gtk_Widget_Record'Class);

   generic
      type Concrete_View_Record is new Root_Gtk_View_Type with private;
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

   type Root_Gtk_View_Type is abstract new Root_View_Type with
      record
         Self       : Gtk_View_Object;
         Top_Widget : Gtk.Widget.Gtk_Widget;
      end record;

   function From_Gtk_Object
     (Object : not null access Glib.Object.GObject_Record'Class)
      return Concrete_View_Access
   is (Concrete_View_Access (Gtk_View_Object_Record (Object.all).View));

   function Object
     (View : Root_Gtk_View_Type)
      return Glib.Object.GObject
   is (Glib.Object.GObject (View.Self));

end Nazar.Views.Gtk_Views;
