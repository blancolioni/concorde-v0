with Gtk.Widget;

package Nazar.Views.Gtk_Views is

   type Root_Gtk_View_Type is abstract new Root_View_Type with private;

   overriding procedure Show (View : in out Root_Gtk_View_Type);

   function Widget
     (View : Root_Gtk_View_Type)
      return Gtk.Widget.Gtk_Widget;

   type Nazar_Gtk_View is access all Root_Gtk_View_Type'Class;

   procedure Initialize
     (View : in out Root_Gtk_View_Type'Class;
      Top  : not null access Gtk.Widget.Gtk_Widget_Record'Class);

private

   type Root_Gtk_View_Type is abstract new Root_View_Type with
      record
         Top_Widget : Gtk.Widget.Gtk_Widget;
      end record;

end Nazar.Views.Gtk_Views;
