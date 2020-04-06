private with Gtk.Drawing_Area;
private with Cairo;

with Nazar.Models.Draw;
with Nazar.Views.Draw;

package Nazar.Views.Gtk_Views.Draw is

   type Nazar_Gtk_Draw_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Draw.Draw_View_Interface
   with private;

   type Nazar_Gtk_Draw_View is access all Nazar_Gtk_Draw_View_Record'Class;

   function Nazar_Gtk_Draw_View_Create
      return Nazar_View;

   function Nazar_Gtk_Draw_View_New
     (Model : not null access Nazar.Models.Draw.Root_Draw_Model'Class)
      return Nazar_Gtk_Draw_View;

private

   type Nazar_Gtk_Draw_View_Record is
     new Nazar_Gtk_View_Record
     and Nazar.Views.Draw.Draw_View_Interface with
      record
         Draw_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Surface   : Cairo.Cairo_Surface := Cairo.Null_Surface;
         Viewport  : Rectangle;
      end record;

   overriding function Class_Name
     (View : Nazar_Gtk_Draw_View_Record)
      return String
   is ("nazar-gtk-draw-view");

   overriding procedure Update_From_Model
     (View : in out Nazar_Gtk_Draw_View_Record);

   overriding procedure Set_Model
     (View  : not null access Nazar_Gtk_Draw_View_Record;
      Model : not null access Nazar.Models.Nazar_Model_Record'Class);

   type Model_Access is
     access all Nazar.Models.Draw.Root_Draw_Model'Class;

   function Draw_Model
     (View : Nazar_Gtk_Draw_View_Record'Class)
      return Model_Access
   is (Model_Access (View.Base_Model));

end Nazar.Views.Gtk_Views.Draw;
