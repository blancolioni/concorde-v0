with Gtk.Main;
with Gtk.Widget;

package body Nazar.Views.Gtk_Views.Application is

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class);

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (View  : in out Nazar_Gtk_Application_View_Record;
      Child :        not null access Nazar.Views.Nazar_View_Record'Class)
   is
   begin
      View.Window.Add
        (Nazar.Views.Gtk_Views.Nazar_Gtk_View (Child).Widget);
   end Append;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   -------------------
   -- Model_Changed --
   -------------------

   overriding procedure Model_Changed
     (View : in out Nazar_Gtk_Application_View_Record)
   is
   begin
      null;
   end Model_Changed;

   ---------------------------------------
   -- Nazar_Gtk_Application_View_Create --
   ---------------------------------------

   function Nazar_Gtk_Application_View_Create return Nazar.Views.Nazar_View is
      View : constant Nazar_Gtk_Application_View :=
        new Nazar_Gtk_Application_View_Record;
   begin
      Gtk.Main.Init;
      Gtk.Window.Gtk_New (View.Window);
      View.Initialize (View.Window);
      View.Window.On_Destroy (Destroy_Handler'Access);
      return Nazar_View (View);
   end Nazar_Gtk_Application_View_Create;

   ------------------------------------
   -- Nazar_Gtk_Application_View_New --
   ------------------------------------

   function Nazar_Gtk_Application_View_New
     (Model : not null access Nazar.Models.Application
        .Nazar_Application_Model_Record'
        Class)
      return Nazar_Gtk_Application_View
   is
      View : constant Nazar_View :=
        Nazar_Gtk_Application_View_Create;
   begin
      View.Set_Model (Model);
      return Nazar_Gtk_Application_View (View);
   end Nazar_Gtk_Application_View_New;

   ----------
   -- Show --
   ----------

   overriding procedure Show
     (View  : in out Nazar_Gtk_Application_View_Record)
   is
   begin
      Root_Gtk_View_Type (View).Show;
      Gtk.Main.Main;
   end Show;

end Nazar.Views.Gtk_Views.Application;
