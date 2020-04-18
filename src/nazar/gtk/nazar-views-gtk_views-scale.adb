with Gtk.Enums;

package body Nazar.Views.Gtk_Views.Scale is

   ---------------------------------
   -- Nazar_Gtk_Scale_View_Create --
   ---------------------------------

   function Nazar_Gtk_Scale_View_Create return Nazar_View is
      View : constant Nazar_Gtk_Scale_View :=
        new Nazar_Gtk_Scale_View_Record;
   begin
      View.Scale :=
        Gtk.Scale.Gtk_Scale_New_With_Range
          (Orientation => Gtk.Enums.Orientation_Horizontal,
           Min         => 0.0,
           Max         => 100.0,
           Step        => 1.0);
      View.Initialize (View.Scale);
      return Nazar_View (View);
   end Nazar_Gtk_Scale_View_Create;

   ------------------------------
   -- Nazar_Gtk_Scale_View_New --
   ------------------------------

   function Nazar_Gtk_Scale_View_New
     (Model : not null access
        Nazar.Models.Numeric.Nazar_Float_Model_Record'Class)
      return Nazar_Gtk_Scale_View
   is
      View : constant Nazar_Gtk_Scale_View :=
        new Nazar_Gtk_Scale_View_Record;
   begin
      View.Scale :=
        Gtk.Scale.Gtk_Scale_New_With_Range
          (Orientation => Gtk.Enums.Orientation_Horizontal,
           Min         => Glib.Gdouble (Model.Minimum),
           Max         => Glib.Gdouble (Model.Maximum),
           Step        => Glib.Gdouble (Model.Step));
      View.Initialize (View.Scale);
      View.Set_Model (Model);
      return View;
   end Nazar_Gtk_Scale_View_New;

   -----------------------
   -- Update_From_Model --
   -----------------------

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Scale_View_Record)
   is
   begin
      View.Scale.Set_Range
        (Min => Glib.Gdouble (View.Scale_Model.Minimum),
         Max => Glib.Gdouble (View.Scale_Model.Maximum));
      View.Scale.Set_Value (Glib.Gdouble (View.Scale_Model.Current));
   end Update_From_Model;

end Nazar.Views.Gtk_Views.Scale;
