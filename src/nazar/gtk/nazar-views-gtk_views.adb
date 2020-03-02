package body Nazar.Views.Gtk_Views is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View : in out Root_Gtk_View_Type'Class;
      Top  :        not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      View.Top_Widget := Gtk.Widget.Gtk_Widget (Top);
   end Initialize;

   ----------
   -- Show --
   ----------

   overriding procedure Show (View : in out Root_Gtk_View_Type) is
   begin
      View.Widget.Show_All;
   end Show;

   ------------
   -- Widget --
   ------------

   function Widget (View : Root_Gtk_View_Type) return Gtk.Widget.Gtk_Widget is
   begin
      return View.Top_Widget;
   end Widget;

end Nazar.Views.Gtk_Views;
