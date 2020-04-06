with Nazar.Gtk_Main;

package body Nazar.Views.Gtk_Views is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View : not null access Nazar_Gtk_View_Record'Class;
      Top  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      View.Initialize;
      View.Top_Widget := Gtk.Widget.Gtk_Widget (Top);
      View.Self :=
        new Gtk_View_Object_Record'(Glib.Object.GObject_Record with
                                      View => Nazar_Gtk_View (View));
      Top.Set_Name (View.Name);
      View.Self.Initialize;
   end Initialize;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed (View : in out Nazar_Gtk_View_Record) is
   begin
      Nazar.Gtk_Main.Schedule_Update (View.Self.View);
   end Model_Changed;

   --------------
   -- Set_Name --
   --------------

   overriding procedure Set_Name
     (View  : in out Nazar_Gtk_View_Record;
      Name  : String)
   is
   begin
      Nazar_View_Record (View).Set_Name (Name);
      View.Widget.Set_Name (Name);
   end Set_Name;

   ----------
   -- Show --
   ----------

   overriding procedure Show (View : in out Nazar_Gtk_View_Record) is
   begin
      View.Widget.Show_All;
   end Show;

   ------------
   -- Widget --
   ------------

   function Widget
     (View : Nazar_Gtk_View_Record)
      return Gtk.Widget.Gtk_Widget
   is
   begin
      return View.Top_Widget;
   end Widget;

end Nazar.Views.Gtk_Views;
