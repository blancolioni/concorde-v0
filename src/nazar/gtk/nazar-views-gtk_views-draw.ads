private with Gtk.Drawing_Area;
private with Cairo;

with Nazar.Models.Draw;
with Nazar.Views.Draw;

package Nazar.Views.Gtk_Views.Draw is

   type Root_Gtk_Draw_View is
     new Root_Gtk_View_Type
     and Nazar.Views.Draw.Draw_View_Interface
   with private;

   type Nazar_Gtk_Draw_View is access all Root_Gtk_Draw_View'Class;

   function Gtk_Draw_View
     (Model : not null access Nazar.Models.Draw.Root_Draw_Model'Class)
      return Nazar_Gtk_Draw_View;

private

   type Root_Gtk_Draw_View is
     new Root_Gtk_View_Type
     and Nazar.Views.Draw.Draw_View_Interface with
      record
         Draw_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Surface   : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Viewport  : Rectangle;
      end record;

   overriding function Class_Name
     (View : Root_Gtk_Draw_View)
      return String
   is ("nazar-gtk-draw-view");

   overriding procedure Model_Changed
     (View : in out Root_Gtk_Draw_View);

   type Model_Access is
     access all Nazar.Models.Draw.Root_Draw_Model'Class;

   function Draw_Model
     (View : Root_Gtk_Draw_View'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Draw;
